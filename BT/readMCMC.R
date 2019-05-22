function(file){
  info<-getInfo(file)
  skp<-startline(file)-1
  
  ratenames<-paste("q", info[,1], info[,2], sep="")
  est<-which(!is.na(info[,3]))
  parnames<-paste("q", info[est,1], info[est,2], sep="")
  parloc<-matrix(c(info[est,1], info[est,2]), ncol=2)
  
  readin<-read.table(file, sep="\t", skip=skp, header=TRUE)
  cols <- colnames(readin)
  
  readin<-readin[,which(cols %in% parnames==TRUE)]
  
  return(readin)
}
