# this function operates on simulated datasets, not a single empirical dataset!

prepData = function(simsets, set, n=500, rmv=NULL){
  
  n = nrow(simsets[[i]])
  options(stringsAsFactors = FALSE)
  
  if(is.character(set)==TRUE){
  
    set = which(names(simsets[[1]]) == set)
    
  }
  
  for (i in 1:n){
  
    dat = simsets[[i]][[set]]
    dat[which(is.na(dat))] = "-"
    
    if (is.null(rmv)==FALSE & is.numeric(rmv) == TRUE) {
    
    ind = sample(1:n), size=rmv)
    dat[ind] = "-"
    
    }
    
    if (i==1) {
    
      data = data.frame(dat)
      colnames(data) = "1"
      
    } else {
    
      data = cbind(data, dat)
      colnames(data)[i] = as.character(i)
      
    }
  }
  
  rownames(data) = rownames(simsets[[1]])
  return(data)
  
}
