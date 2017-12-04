select_by_corr <- function(df, aimst, method = 'kendall', datecol = T, only_slope = T, max_st = 3) {
  if (sum(names(df)[1:3] == c('Main_station','Support_station','Correlation_value')) == 3) {
    df <- df
  }else{
    df <- corr_summary(df, method = method, datecol = datecol, only_slope = only_slope)
  }
  if (is.numeric(aimst)) {
    aimst <- unique(as.character(df[,'Main_station'])[aimst])
  }
  temp <- df[df['Main_station'] == aimst,]
  possible_st <- as.character(temp[,'Support_station'])
  corrs <- temp[,'Correlation_value']
  possible_st <- possible_st[match(sort(corrs, decreasing = T), corrs)]
  corrs <- corrs[match(sort(corrs, decreasing = T), corrs)]
  if (length(possible_st) > max_st) {
    # return
    data.frame(stations = c(aimst, possible_st), correlation = c(0,corrs),
               selection = c('Base',rep('Selected',times = max_st),rep('Not selected',times = length(possible_st) - max_st)))
  }else{
    # return
    data.frame(stations = c(aimst, possible_st), correlation = c(0,corrs),
               selection = c('Base',rep('Selected',times = length(possible_st))))
  }
}