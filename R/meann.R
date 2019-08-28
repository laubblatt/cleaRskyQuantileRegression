#' functions to calculate the average when a defined number of data points is available
#'
#' @export meann
#'
NULL

meann <- function(x,nmin,...) {
  #' an average mean which requires a minimum of data points nmin

  #'  needed if missing data is missing :)
  #' @param x vector of numeric values
  #' @param nmin minimum of data points accepted to calculate the mean (integer)
  #' @param ... further arguments to mean() remember especially to set na.rm=TRUE
  #' @version 20141208 add non-missing data lower nmin
  #' @author Maik Renner mrenner [at] bgc-jena.mpg.de
  #' @examples
  #' x = 1:10
  #' meann(x,nmin = 11)
  #' meann(NULL,nmin = 11)
  #' x = c(1,2,NA,Inf,NaN)
  #' meann(x,2,na.rm=TRUE)
  #' x = c(1,2,NA,NA,NaN)
  #' meann(x,2,na.rm=TRUE)
  #' meann(x,3,na.rm=TRUE)
  #' sum(!is.na(x))

  # http://stackoverflow.com/questions/12125364/why-does-median-trip-up-data-table-integer-versus-double

  n = length(x)
  nnona = sum(!is.na(x))
  if (n < nmin | nnona < nmin) out = NA
  else out = mean(x,...)
  return(as.double(out))  ### data.table needs consitent output ... for data allocation
}
