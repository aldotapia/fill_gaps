select_basin <- function(df, sta, field, basin, datecol = T){
  if (datecol == T) {
    df <- df[,c(TRUE,sta@data[,field] == basin)]
    df
    }else{
      df <- df[,sta@data[,field] == basin]
    }
  df
}