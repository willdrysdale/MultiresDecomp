#' Read n-csv
#' 
#' Shorthand for \code{map_df(filenames,read.csv,...)} so it can easily be passed to \code map
#' 
#' @param ncsv vector of filenames
#' 
#' @author W. S. Drysdale
#' 
#' @export

read_ncsv = function(ncsv,...){
  map_df(ncsv,read_csv,...)
}