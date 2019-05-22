meanRates = function(btsum){
  parnames = colnames(btsum$concat)
  out = matrix(NA, nrow = length(parnames), ncol = 8,
               dimnames = list(parnames, colnames(btsum$summary)[4:11]))
  colnames(out)
  btsum = btsum$summary[,-(1:2)]
  
  for(i in 1:length(parnames)){
    ind = which(btsum$par == parnames[i])
    par = btsum[ind,2:ncol(btsum)]
    out[i,] = colMeans(par)
  }
  return(out)
}
