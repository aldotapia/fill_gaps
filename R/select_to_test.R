select_to_test <- function(df, datecol_pos = 1, aimst = 1, valid_values = 60,
                           start_date = '1970-01', end_date = '2016-12') {
  df <- cbind.data.frame(df[,datecol_pos], df[,-datecol_pos])
  if (is.numeric(aimst)) {
    aimst <- names(df)[aimst + 1]
  }
  dates <- df[,1]
  # Select all data where aim station has values (prior to train)
  temp <- df[!is.na(df[,aimst]),]
  # Select data from start_date to end_date where aim station has NA values
  temp2 <- df[is.na(df[,aimst]) &
                1:dim(df)[1] %in% which(dates == start_date):which(dates == end_date),]
  # Test stations with complete data in NA period of aim station
  col_sum <- apply(temp2, 2, function(x)sum(is.na(x)))
  row_sum <- apply(temp2, 1, function(x)sum(is.na(x)))
  print(temp2)
  print(col_sum)
  print(row_sum)
}