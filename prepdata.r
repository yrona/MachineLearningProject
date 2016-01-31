## ---- prepfunctions ----
NA.Percentage <- function(x) {
  #Returns the percentage of observations in a vector that are NA
  sum(is.na(x))/length(x)
}

cols.good.NA.Percentages <- function(x, min.threshold = 0.10, cols.to.pass =c()) {
  #Is passed:
  #   a data frame,
  #   a minimum threshold below which columns are excluded, and
  #   a vector of columns to allow even if they don't meet the threshold.
  #Returns a vector of columns that have fewer NA's than the threshold
  
  NA.percentages <- data.frame(sapply(x,NA.Percentage))
  colnames( NA.percentages ) <- "ratio"
  
  good.cols <- subset(NA.percentages , ratio < min.threshold)
  
  if (length(cols.to.pass) > 0) {
    #First we find the rows associated with the columns we will pass 
    #(ignoring columns that don't exist)
    pass.cols <- subset(NA.percentages, row.names(NA.percentages) %in% cols.to.pass)
    
    #We then identify any of the rows that didn't make the cut above
    candidate.cols <-rownames(pass.cols)
    missing.candidates <- candidate.cols[!candidate.cols %in% rownames(good.cols)]
    
    #Finally we concatenate the two sets
    old.row.names <- rownames(good.cols)
    good.cols <- rbind(good.cols,pass.cols[missing.candidates,])
    
    rownames(good.cols) <- c(old.row.names,missing.candidates)
    
  }
  
  return(rownames(good.cols))
}
