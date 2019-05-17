concatenateBT<-function(files, info){

files<-list.files(pattern="Log.txt")
modelnames<-substr(files, start=9, stop=11)
nmodel<-c(1:length(files))

ratenames<-paste("q", info[,1], info[,2], sep="")
est<-which(is.na(info[,3]))
parnames<-paste("q", info[est,1], info[est,2], sep="")
parloc<-matrix(c(info[est,1], info[est,2]), ncol=2)
model<-getModelDesign(info, nstates)
npar<-length(parnames)


for (i in 1:length(files)){

  cols<-as.matrix(read.table(files[i], sep="\t", header=FALSE, nrow=1))[1,]
  
  
  if (length(cols[which(parnames %in% cols==TRUE)])==0){
    
    c2<-as.matrix(read.table(files[i], sep="\t", skip=72, header=FALSE, nrow=1))[1,]
    names(c2)<-NULL
    
    c2[which(c2 %in% parnames==FALSE)]<-"NULL"
    c2[which(c2 %in% parnames==TRUE)]<-"numeric"
    
    readin<-read.table(files[i], sep="\t", skip=72, header=TRUE, colClasses=c2)
    
  } else {
    c2<-cols
    names(c2)<-NULL
    c2[1:length(c2)]<-"NULL"
    c2[which(cols %in% parnames==TRUE)]<-"numeric"
    
    
    readin<-read.table(files[i], sep="\t", header=TRUE, colClasses = c2)
    
  }
  
  print(paste("Finished reading file", i))
  
  q<-c(0.05, 0.25, 0.5, 0.75, 0.95)
  tmp<-summary(as.mcmc(readin), quantiles=q)
  mcmcsum<-cbind(tmp$statistics[,1:3], tmp$quantiles)
  num<-rep(nmodel[i], times=npar)
  prior<-rep(modelnames[i], times=npar)
  
  if (i==1){
    out<-cbind("n"=num, "prior"=prior, mcmcsum)
  } else {
    add<-cbind("n"=num, "prior"=prior, mcmcsum)
    out<-rbind(out, add)
  }
  
}

return(out)

}
