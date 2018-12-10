#' Group Vector
#' 
#' Gathers sequential items in a vector (e.g file names) into a list of vectors, of length n
#' 
#' @param vec vector
#' @param n length of groups
#' 
#' @export
#' 
#' @author W. S. Drysdale

group_vec = function(vec,n){
  rem = length(vec) %% n
  
  #trim the vector if there is remainder when grouping
  if(rem != 0)
    vec = files[1:(length(vec)-rem)]
  
  vec = matrix(vec,nrow = n) %>% t
  
  vec = plyr::alply(vec,1,c)
  
  vec
}