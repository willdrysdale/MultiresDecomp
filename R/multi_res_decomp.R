#' Multiresolution Decomposition
#' 
#' This function performs the actual decomposition, following the steps presented in Vickers and Marht 2003 \cr
#' \cr
#' A simple multiresolution spectra is produced when only col1 is defined, and a cospectra is produced if col2 is also defined
#' 
#' @param df Data.frame containing the timeseries to be decomposed
#' @param col1 Name of first column
#' @param col2 Optional name of second column, when supplied cospectra of col1*col2 is produced
#' @param time_res The current time resolution of the series in seconds.
#' @param inc_w_n  Default FALSE, when TRUE the intersitial "w_n" steps are returned. In this case function returns a list
#' 
#' @return data.frame of spectra
#' 
#' @export
#' 
#' @author W. S. Drysdale 

multi_res_decomp = function(df,col1,col2 = NULL,time_res = 0.2,inc_w_n = F){
  
  df = as.data.frame(df)
  
  d = df[,col1]
  mapped = map_to_2M(d,time_res)
  d = mapped$series$y
  
  if(!is.null(col2)){
    d2 = df[,col2]
    mapped = map_to_2M(d2,time_res)
    d2 = mapped$series$y
  }
  
  time_res = mapped$res
  M = mapped$M
  
  m = M:0 #m counts backwards through the stages from M to 0
  mr_spectra = vector("double",length(m))
  w_n_all = list()
  for(i in 0:M){ #MR decomp is performed m times
    #data must first be split into 2^(i-1)
    d_seg = n_segs(d,i)
    w_n = purrr::map_dbl(d_seg,mean,na.rm = T) # n segment means as vector
    
    if(!is.null(col2)){# if calculating cospectra
      d2_seg = n_segs(d2,i)
      psi_n = purrr::map_dbl(d2_seg,mean,na.rm = T)
      D_w = smam(w_n,M,m[i+1],psi_n)
      
    }else
      D_w = smam(w_n,M,m[i+1]) # calculate second moment about mean
    
    w_n_all[[i+1]] = w_n
    mr_spectra[i+1] = D_w
    #reform d subtracting the means from each segment
    d_seg_sub = list()
    for(j in 1:length(d_seg))
      d_seg_sub[[j]] = d_seg[[j]] - w_n[j]
    
    d = unlist(d_seg_sub)
  }
  names(w_n_all) = m
  
  if(inc_w_n){
    ret = list(
      w_n = w_n_all,
      mr_spectra = data.frame(scale = 2^(m[1:(length(m)-1)])*time_res,
                              d_w = mr_spectra[2:length(mr_spectra)])
    )
    ret$mr_spectra$index = 1:nrow(ret$mr_spectra)
    ret$mr_spectra$csum = cumsum(ret$mr_spectra$d_w)
  }else{
    ret = data.frame(scale = 2^(m[1:(length(m)-1)])*time_res,
                     d_w = mr_spectra[2:length(mr_spectra)]
    )
    ret$index = 1:nrow(ret)
    ret$csum = cumsum(ret$d_w)
  }
  #return
  ret
}