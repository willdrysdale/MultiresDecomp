#' n_segs
#' 
#' Split the series into the correct number of segments for this iteration of the MRD
#' 
#' @param d vector of data
#' @param i 2^i is the number of segments returned
#' 
#' @return list of segments
#' 
#' @export
#' 
#' @author W. S. Drysdale

n_segs = function(d,i){
  nsegs = 2^(i)
  l = length(d)
  width = l/nsegs
  
  segs = list()
  
  for(j in 1:nsegs){
    start = ((j*width)-(width))+1
    end = j*width
    segs[[j]] = d[start:end]
  }
  
  segs
  
}