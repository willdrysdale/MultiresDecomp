#' Calculate M
#' 
#' Determines the maximum value of M, where 2^M < N (length of original series)
#' 
#' @param d vector of points in the series
#' 
#' @export
#' 
#' @author W. S. Drysdale

calc_M = function(d){
  M = 0
  while(2^M <= length(d))
    M = M+1
  M-1
}


