select_by_dist <- function(df, aimst, sp, code_field, datecol = T, datecol_pos = 1, max_st = 3,
                           start_date = '1970-01', end_date = '2016-12') {
  results <- list()
  dates <- df[,datecol_pos]
  df <- df[,-datecol_pos]
  if (!is.numeric(aimst)) {
    aimst <- which(names(df) == aimst)
  }
  if (!require("sp")) install.packages("sp"); library(sp)
  # Select all data where aim station has values (prior to train)
  temp <- df[!is.na(df[,aimst]),]
  # Select data from start_date to end_date where aim station has NA values
  temp2 <- df[is.na(df[,aimst]) &
                   1:dim(df)[1] %in% which(dates == start_date):which(dates == end_date),]
  df2 <- cbind.data.frame(dates,df)[which(dates == start_date):which(dates == end_date),]
  names(df2)[1] <- 'Date'
  df2[,'Date'] <- as.character(df2[,'Date'])
  # Test stations with complete data in NA period of aim station
  logic_test <- apply(temp2, 2, function(x)sum(is.na(x)) == 0)
  # if there is no possible filling station, go to else
  if (sum(logic_test) > 0) {
    # code of aim station
    aim_st <- names(df)[aimst]
    # codes of possible filling stations
    possible_st <- names(temp)[logic_test]
    # distance from aim station to filling stations
    dists <- spDistsN1(sp[sp@data[,code_field] %in% possible_st,],sp[sp@data[,code_field] %in% aim_st,])
    # sort stations by distance
    possible_st <- possible_st[match(sort(dists), dists)]
    dists <- dists[match(sort(dists), dists)]
    if (length(possible_st) > max_st) {
      # return
      rst <- data.frame(stations = c(aim_st, possible_st), distance = c(0,dists),
               selection = c('Base',rep('Selected',times = max_st),rep('Not selected',times = length(possible_st) - max_st)),stringsAsFactors = F)
    }else{
      # return
      rst <- data.frame(stations = c(aim_st, possible_st), distance = c(0,dists),
                 selection = c('Base',rep('Selected',times = length(possible_st))),stringsAsFactors = F)
    }
    rst
    results[[1]] <- rst
    selection <- rst[which(rst[,'selection'] %in% c('Base','Selected')),1]
    if (datecol == T) {
      results[[2]] <- df2[,c('Date',selection)]
    }else{
      results[[2]] <- df2[,selection]
    }
    results
  }else{
    print("No station with data to fill!")
  }
}