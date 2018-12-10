#' Second Moment about the Mean
#' 
#' Calculate the second moment about the mean for a vector of points (or the cospectral equivalent if psi_n is supplied)
#' 
#' @param w_n vector of points from current stage of decompostion
#' @param M Maximum value of m, 2^M is the length of the original series
#' @param m is the current value of m for this iteration
#' @param psi_n second vector of points for cospectral case
#' 
#' @export
#' 
#' @author W. S. Drysdale

smam = function(w_n,M,m,psi_n = NULL){
  Mlessm = M-m
  if(is.null(psi_n))
    smam = (1/(2^Mlessm))*sum(w_n^2)
  else
    smam = (1/(2^Mlessm))*sum(w_n*psi_n)
  
  smam
}