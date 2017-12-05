# agregar seleccionar por fecha
select_by_corr <- function(df, aimst, method = 'kendall', datecol = T, datecol_pos = 1, only_slope = T, max_st = 3,
                           start_date = '1970-01', end_date = '2016-12') {
  results <- list()
  df <- cbind.data.frame(df[,datecol_pos], df[,-datecol_pos])
  if (is.numeric(aimst)) {
    aimst <- names(df)[aimst + 1]
  }
  # Select data from start_date to end_date where aim station has NA values
  df2 <- df[which(df[,1] == start_date):which(df[,1] == end_date),]
  names(df2)[1] <- 'Date'
  df2[,'Date'] <- as.character(df2[,'Date'])
  df <- corr_summary(df2, method = method, datecol = datecol, only_slope = only_slope)
  temp <- df[df['Main_station'] == aimst,]
  possible_st <- as.character(temp[,'Support_station'])
  corrs <- temp[,'Correlation_value']
  possible_st <- possible_st[match(sort(corrs, decreasing = T), corrs)]
  corrs <- corrs[match(sort(corrs, decreasing = T), corrs)]
  if (length(possible_st) > max_st) {
    # return
    rst <- data.frame(stations = c(aimst, possible_st), correlation = c(0,corrs),
               selection = c('Base',rep('Selected',times = max_st),rep('Not selected',times = length(possible_st) - max_st)),stringsAsFactors = F)
  }else{
    # return
     rst <- data.frame(stations = c(aimst, possible_st), correlation = c(0,corrs),
                               selection = c('Base',rep('Selected',times = length(possible_st))),stringsAsFactors = F)
  }
  results[[1]] <- rst
  selection <- rst[which(rst[,'selection'] %in% c('Base','Selected')),1]
  if (datecol == T) {
    results[[2]] <- df2[,c('Date',selection)]
  }else{
    results[[2]] <- df2[,selection]
  }
  results
}