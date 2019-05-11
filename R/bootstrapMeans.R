# just a simple wrapper to get mean trait data from a bootstrapped dataset

bootstrapMeans<-function(x){
  x<-x[complete.cases(x),]
  return(rowMeans(x))
}
