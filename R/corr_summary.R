corr_summary <- function(df, method = 'kendall', datecol = T, only_slope = T) {
  # check if there is a date column
  if (datecol == T) {
    df <- df[,-1]
  }
  # compute correlation
  corre <- cor(df, use = 'na.or.complete' ,method = method)
  # create a list to store results
  s_list <- list()
  for (i in 1:dim(df)[2]) {
    temp <- df[,i] # data fields
    temp_list <- list() # create temporal list
    for (j in 1:dim(df)[2]) { 
      temp2 <- df[,j] # data field
      if (only_slope == T) {
        temp_model <- lm(temp ~ temp2 - 1) # lineal model with center in origin
        coef <- coefficients(temp_model) # extract slope
        corr_value <- corre[i,j] # extract correlation by station
        n_obs <- sum(complete.cases(temp,temp2)) # count number of valid observatioms
        main_st <- names(df)[i] # extract name of aim station
        supp_st <- names(df)[j] # extract name of support station
        # create summary of listed above
        temp_list[[j]] <- data.frame(Main_station = main_st, Support_station = supp_st, Correlation_value = corr_value, LM_slope = coef, n_observation = n_obs)
      }else{
        temp_model <- lm(temp ~ temp2) # lineal model with center in origin
        coef <- coefficients(temp_model) # extract slope
        corr_value <- corre[i,j] # extract correlation by station
        n_obs <- sum(complete.cases(temp,temp2)) # count number of valid observatioms
        main_st <- names(df)[i] # extract name of aim station
        supp_st <- names(df)[j] # extract name of support station
        # create summary of listed above
        temp_list[[j]] <- data.frame(Main_station = main_st, Support_station = supp_st, Correlation_value = corr_value, LM_interc = coef[1],LM_slope = coef[2], n_observation = n_obs)
      }
    }
    # save data.frame with summary for station i
    s_list[[i]] <- do.call(rbind.data.frame,temp_list)
  }
  # create a unique data.frame with all stations
  s_list <- do.call(rbind.data.frame, s_list)
  # delete correlation with the same station
  s_list <- s_list[-which(s_list$Main_station == s_list$Support_station),]
  # delete row name only for aesthetics
  row.names(s_list) <- NULL
  # return
  s_list
}