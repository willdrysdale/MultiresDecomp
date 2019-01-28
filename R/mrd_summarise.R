#' MRD and Summarise
#' 
#' Runs the Multiresolution decompostion over multiple files and returns the mean of all decompositions \cr
#' Currently only implements cospectral case
#' 
#' @param dat List of data.frames to map \code{multi_res_decomp} over
#' @param id string identifying this run - so that results can be bound and compared
#' @param col1 name or vector of first param. if this is shorter than col2, the first value will be repeated
#' @param col2 name or vector of second param
#' @param group_names optional, what should each output be named. Will be ignored if it is of different length to col2
#' 
#' @export
#' 
#' @author W. S. Drysdale

mrd_and_summarise = function(dat,
                             id = "1hour",
                             col1 = "w_met",
                             col2 = c("FD_mole_NO1","FD_mole_NO2","T_air","uv_met"),
                             group_names = NULL,
                             time_res = 0.2,
                             inc_w_n = F){
  
  #When col1 is not long enough, repeat the first instance only
  if(length(col1) != length(col2))
    col1 = rep(col1[1],length(col2))
  
  decomp = list()
  for(i in 1:length(col1)){
    #if names are not supplied, create them
    if(!is.null(group_names) & length(group_names) == length(col2))
      grp_nm = group_names[i]
    else
      grp_nm = paste0(col1[i],"__",col2[i])
    
    print(paste0("Decomposing ",col2[i]," wrt ",col1[i]))
    
    #run decomp and summarise
    decomp[[i]] = map_df(dat,multi_res_decomp,col1 = col1[i],col2 = col2[i],time_res = time_res, inc_w_n = inc_w_n) %>%
      mutate(grp = grp_nm) %>% 
      group_by(grp,index) %>% 
      summarise_all(mean,na.rm = T) %>% 
      mutate(d_w_norm = scales::rescale(d_w),
             csum_norm = d_w_norm %>%
               rev %>%
               cumsum,
             id = id
      )
    
  }
  #return
  if(!is.null(group_names) & length(group_names) == length(decomp))
    names(decomp) = group_names
  else
    names(decomp) = paste0(col1,"__",col2)
  
  print("Complete")
  decomp
  
  
}
