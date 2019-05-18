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
  est<-which(is.na(info[,3]))
  parnames<-paste("q", info[est,1], info[est,2], sep="")
  parloc<-matrix(c(info[est,1], info[est,2]), ncol=2)
  
  adj<-matrix(nrow=n, ncol=n)
  diag(adj)<-0
  
  for (i in 1:nrow(info)){
    
    x = info[i,1]
    y = info[i,2]
    
    if (is.na(info[i,3])==TRUE){
      adj[x,y]<-ratenames[i]
    } else if (info[i,3]==0){
      adj[x,y]<-0
    } else {
      adj[x,y]<-paste("q", info[i,3], info[i,4], sep="")
    }
  }
  return(adj)
}
