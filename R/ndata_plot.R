ndata_plot <- function(df, date_format = 'year-month', date_col = 1, comprss_colname = T,
                       comprss_colfctor = 2, comprss_rowname = T, comprss_rowfctor = 2) {
  if (!require("lattice")) install.packages("lattice"); library(lattice)
  if (date_format == 'year-month') {
    start <- strsplit(df[1,date_col],split = '-')[[1]][1]
    end <- strsplit(df[dim(df)[1],date_col],split = '-')[[1]][1]
    df <- df[,-date_col]
    ndata <- apply(df,MARGIN = 1,FUN = function(x){sum(!is.na(x))})
    if (!require("lattice")) install.packages("lattice"); library(lattice)
    ndata <- matrix(ndata,nrow = 12, byrow = F)
    colnames(ndata) <- start:end
    rownames(ndata) <- month.abb
    if (comprss_colname == T) {
      colnames(ndata)[-seq(1,length(colnames(ndata)),comprss_colfctor)] <- ''
    }
    if (comprss_rowname == T) {
      rownames(ndata)[-seq(1,length(rownames(ndata)),comprss_rowfctor)] <- ''
    }
    print(levelplot(ndata, scales = list(tck = 0, x = list(rot = 90)),
                                         col.regions = colorRampPalette(c("red3", "orange", "darkgoldenrod1", 
                                                                         "yellow", "darkolivegreen2", "green")),
                                         xlab = NULL, ylab = NULL))
  }
}