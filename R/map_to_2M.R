#' Map to 2^M
#' 
#' Multiresolution Decomposition requires the length of the series to be 2^M points, where M is an interger > 0. \cr
#' For the majority of series this is not the case. map_to_2M interpolates the series on the nearest 2^M length series, where 2^M < N.
#' 
#' @param d vector of points in the series
#' @param time_res time resolution of original series in seconds
#' 
#' @export
#' 
#' @author W. S. Drysdale

map_to_2M = function(d,time_res = 0.2){
  M = calc_M(d)
  N = length(d)
  
  new_res = ((N-1)*time_res)/((2^M)-1)
  
  d_interp = approx(d,n = 2^M)
  
  list(series = d_interp,res = new_res,M = M)
}