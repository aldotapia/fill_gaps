############################################
############################################
###                                      ###
###   PRECIPITATION STATIONS SELECTION   ###
###                                      ###
############################################
############################################

# Necessary libraries
library(hydroTSM)
library(lattice)
library(dplyr)
library(corrplot)

# Read precipitation data
datos <- read.csv(file = "data/cr2_prAmon_2017_ghcn/cr2_prAmon_2017_ghcn.txt",
                  header = T,skip = 14,sep = ",",na.strings = -9999)

# Read stations metadata
estaciones <- read.csv(file = "data/cr2_prAmon_2017_ghcn/cr2_prAmon_2017_stations_ghcn.txt.txt")

# Assing name by code station
names(datos) <- c('Fecha',estaciones$codigo_estacion)
# Select data from 1935 (before there isn't data)
datos <- datos[421:dim(datos)[1],] # data from 1935-01

############################
### Limarí River Basin analysis
############################

# Create logic test to select stations inside Limarí river basin
testlogico <- c(TRUE,estaciones$nombre_cuenca == 'Rio Limari')

# Apply selection with logic test
datos <- datos[,testlogico]
remove(testlogico) # useless now

# Count valid data per month
ndata <- apply(datos[,2:39],MARGIN = 1,FUN = function(x){sum(!is.na(x))})

# Create matrix for graphical purposes
ndata <- matrix(ndata,nrow=12,byrow=F) # warning by number of rows expected

# Assign column name
colnames(ndata) <- 1935:2017

# Assign row name
rownames(ndata) <- month.abb

# Valid data plot
matrixplot(ndata)
remove(ndata) # useless now

# Count valid data per station
datos_validos <- apply(datos[,2:39],2,function(x) sum(!is.na(x)))

# Select stations with at least 30 years of valid data (360 months), not necessary continuos
datos_validos <- datos_validos[datos_validos > 360] # 360 <- 1 climatic period of data

# extract code of selected stations
seleccion <- names(datos_validos)

# Use only selected stations
datos <- datos[,c('Fecha',seleccion)]
remove(seleccion) # useless now

# compute correlation with Kendall (more used method for precipitation)
corre <- cor(datos[2:(length(datos_validos)+1)],use='na.or.complete',method='kendall')

# correlation plot
corrplot(corre,type = "lower", method='pie', tl.col = "black", tl.srt = 15,
         sig.level = .99)

# summary for lineal model by station
summary_stations <- list()

# start the loop
for(i in 1:length(datos_validos)){
  temp <- datos[,i+1] # data fields
  
  temp_list <- list() # create temporal list
  
  for(j in 1:(length(datos_validos))){ 
    temp2 <- datos[,j+1] # data field
    temp_model <- lm(temp ~ temp2 - 1) # lineal model with center in origin
    coef <- coefficients(temp_model) # extract slope
    corr_value <- corre[i,j] # extract correlation by station
    n_obs <- sum(complete.cases(temp,temp2)) # count number of valid observatioms
    main_st <- names(datos)[i+1] # extract name of aim station
    supp_st <- names(datos)[j+1] # extract name of support station
    # create summary of listed above
    temp_list[[j]] <- data.frame(Main_station=main_st,Support_station=supp_st,Correlation_value=corr_value,LM_coefficient=coef,n_observation=n_obs)
  }
  # save data.frame with summary for station i
  summary_stations[[i]] <- do.call(rbind.data.frame,temp_list)
}

# create a unique data.frame with all stations
summary_stations <- do.call(rbind.data.frame,summary_stations)
remove(i,j,temp,temp2,temp_list,temp_model,main_st,supp_st,n_obs,coef,corr_value) # ya no se usa

# delete correlation == 1 (aim and support station equal)
summary_stations <- summary_stations[-which(summary_stations$Correlation_value == 1),]

# delete row name only for aesthetics
rownames(summary_stations) <- NULL

######################################
######################################
###                                ###
###   FILLING PRECIPITATION GAPS   ###
###                                ###
######################################
######################################

# Necessary libraries
library(qrnn)
library(sp)
library(raster)
library(caret)
library(tmap)

# convert stations data.frame to SpatialPointsDataFrame
estaciones <- SpatialPointsDataFrame(data.frame(estaciones$longitud,estaciones$latitud),
                                     data=estaciones,
                                     proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Select only stations under analysis
estaciones <- estaciones[estaciones$codigo_estacion %in% names(datos),]

# to store models
qrnn_list <- list()

# to store results
data_pred_list <- list()

# copy data to store predicted values
datos_pred <- datos

# Start bucle
for(i in 1:length(datos_validos)){
  # Select all data where aim station has values (prior to train)
  temp <- datos[!is.na(datos[,i+1]),-1]
  
  # Select data from 1970-01 to 2016-12 where aim station has NA values
  temp2 <- datos[is.na(datos[,i+1]) &
                   1:dim(datos)[1] %in% which(datos$Fecha == '1970-01'):which(datos$Fecha == '2016-12'),-1]
  
  # Test stations with complete data in NA period of aim station
  logic_test <- apply(temp2,2,function(x)sum(is.na(x))==0)
  
  # if there is no possible filling station, go to else
  if(sum(logic_test) > 0){
    
    # code of aim station
    aim_st <- names(temp)[i]
    
    # codes of possible filling stations
    possible_st <- names(temp)[logic_test]
    
    # distance from aim station to filling stations
    dists <- spDistsN1(estaciones[estaciones$codigo_estacion %in% possible_st,],estaciones[estaciones$codigo_estacion %in% aim_st,])
    
    if(length(possible_st) > 3){
      pre_poss <- possible_st
      possible_st <- possible_st[which(dists %in% sort(dists)[1:3])]
    }else{pre_poss <- possible_st}
    
    # plot stations
    # circles are all stations
    # violet crosses are possible filling stations
    # blue crosses are selected filling stations
    # red cross is aim station
    qtm(shp=estaciones,symbols.shape = 1, scale=0.8) + tm_style_white()+
      tm_grid(n.x=10,n.y=10,col = "gray80") + tm_format_Europe(inner.margins=0.1,frame=F)+
      tm_xlab("Longitude") +
      tm_ylab("Latitude") +
      qtm(estaciones[estaciones$codigo_estacion %in% pre_poss,],dots.col='violet',symbols.shape = 3) +
      qtm(estaciones[estaciones$codigo_estacion %in% possible_st,],dots.col='blue',symbols.shape = 3)+
      qtm(shp=estaciones[estaciones$codigo_estacion %in% aim_st,],dots.col='red',symbols.shape = 4) +
      tm_compass() + tm_scale_bar()
    
    # create a data.frame to train ANN
    df_train <- data.frame(datos[,c(aim_st,possible_st)],monthly_mean=round(apply(X = datos[,possible_st],MARGIN = 1,FUN = function(x) mean(x,na.rm=T)),1))
    
    # set names (station code are numbers... Didn't work as label)
    names(df_train) <- c(letters[1:(dim(df_train)[2]-1)],'monthly_mean')
    
    # set complete cases for all observation
    df_train_ann <- df_train[complete.cases(df_train),]
    
    # create formula to train model (really simple a ~ b + c + d + monthly mean)
    formula_temp <- as.formula(paste0('a',' ~ ',paste(names(df_train)[2:dim(df_train)[2]],collapse = ' + ')))
    
    # aim_st <- names(datos)[i+1]
    # 
    # na_values <- which(is.na(temp))
    # 
    # LM_coeffs <- summary_stations %>% filter(Main_station == aim_st) %>% arrange(desc(Correlation_value)) %>% .[1:5,c('Support_station','LM_coefficient')]
    # 
    # To_fill <- datos[,as.character(LM_coeffs$Support_station)]
    # 
    # To_fill %<>% map2_df(.y = LM_coeffs$LM_coefficient,.f = ~.x*.y)
    # 
    # #
    
    # fit with cross-validation in caret
    fitControl <- trainControl(method = 'cv', number = 6, summaryFunction=defaultSummary)
    
    # create a grid to optimize model (more than 3 neurons is innapropiated to n-observations)
    Grid <- expand.grid(n.hidden = 1:3, penalty = seq(0,0.1,0.05), bag=c(T,F))
    
    set.seed(123)
    
    # train model
    fit.qrnn <- train(formula_temp, data=df_train_ann, method = 'qrnn',
                      trControl=fitControl,tuneGrid=Grid,metric='RMSE',maximize=FALSE,trace=F)
    
    print(paste0('QRNN number ',i,' completed!'))
    
    qrnn_list[[i]] <- fit.qrnn
    
    # new_values <- round(predict(fit.gbm,df_train,na.action=na.pass),1)
    # 
    # plot(df_train$a,new_values,ylab='modeled precip. (mm)',xlab='observed precip. (mm)')
    
    # predict missing values
    new_values <- round(predict(fit.qrnn,df_train[which(as.numeric(rownames(df_train)) %in% as.numeric(rownames(temp2))),]),1)
    
    # if one value is lt 0 (i.e: -0.1 (could be)), set to 0
    new_values[new_values < 0] <- 0
    
    # replace NA
    datos_pred[is.na(datos_pred[,i+1]) &
                 1:994 %in% which(datos$Fecha == '1970-01'):which(datos$Fecha == '2016-12'),i+1] <- new_values
    
    # store date and predicted value for each station
    data_pred_list[[i]] <- datos_pred[which(is.na(datos_pred[,i+1]) != is.na(datos[,i+1])),c(1,i+1)]
    
  }else{
    qrnn_list[[i]] <- 'Did not find possible filling stations'
    data_pred_list[[i]] <- 'Did not find possible filling stations'
  }
  
  # remove garbage
  remove(temp,temp2,logic_test,aim_st,possible_st,dists,pre_poss,df_train,df_train_ann,formula_temp,fitControl,fit.qrnn,new_values,Grid)
}

remove(i) # useless now

# list objects to save them manually
to_save <- ls()

# save each object as serialized data
for(i in 1:length(to_save)){
  saveRDS(to_save[i],
          paste0('/Users/aldotapia/Desktop/Proyectos/Landsat_sequia/SPI_analisis/pp_QRNN_data/',
                 to_save[i],'.rds'))
}

remove(i,to_save) # useless now

# Save all object to a image file
save.image('/Users/aldotapia/Desktop/Proyectos/Landsat_sequia/SPI_analisis/pp_QRNN_data/Total_Data.RData')

#########################
#########################
###                   ###
###   COMPUTING SPI   ###
###                   ###
#########################
#########################

# Necessary libraries
library(SPEI)

# Select data (in this case, 1970-01 to 2016-12 period)
data_analysis <- datos_pred[1:dim(datos_pred)[1] %in% which(datos_pred$Fecha == '1970-01'):which(datos_pred$Fecha == '2016-12'),]

# create a time series object
{data_ts <- ts(data = data_analysis[,16],
               start = c(1970,1),
               end = c(2016,12),
               frequency = 12)
  
  test <- spi(data = data_ts, scale = 24,ref.start=c(1971,1), ref.end=c(2000,12))#,kernel=list(type='gaussian', shift=0))
  test$fitted[is.na(test$fitted)] <- 0
  plot(test)
}
