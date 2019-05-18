# takes discrete trait modeling output from BayesTraits .Log file
# constructs the adjacency matrix that describes transition rate parameterization
# output is pre-formatted to supply to various R functions (e.g., ace() in 'ape')

getModelDesign<-function(x){
  
  if (is.data.frame(x)==TRUE){
    info <- x
  } else {
    info <- getInfo(x)
  }
  
  n<-max(info$rPar)
  
  ratenames<-paste("q", info[,1], info[,2], sep="")
  valnames<-paste("q", info$rVal, info$cVal, sep="")
  
  freepar<-ratenames[ratenames %in% valnames]
  
  parnames<-levels(as.factor(ratenames))
  
  zeroes<-which(is.na(info[,3]))
  nonzero<-which(!is.na(info[,3]))
  
  adj<-matrix(NA, nrow=n, ncol=n)
  
  upper<-which(info$rPar < info$cPar)
  lower<-which(info$rPar > info$cPar)
  
  valnames[zeroes]<-NA
  
  for (i in 1:length(valnames)){
    x<-info$rPar[i]
    y<-info$cPar[i]
    
    adj[x,y]<-valnames[i]
  }
  return(adj)
}
