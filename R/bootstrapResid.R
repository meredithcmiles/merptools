# compute standardized residuals from 2 bootstrapped trait datasets
# returns data in identical structure as bootstrapTrait()

bootstrapResid<-function(y, x){
  y<-y[complete.cases(y), ]
  x<-x[complete.cases(x), ]
  
  x<-x[-which(rownames(x) %in% rownames(y) == FALSE), ]
  y<-y[-which(rownames(y) %in% rownames(x) == FALSE), ]
  
  out<-matrix(NA, nrow=nrow(y), ncol=ncol(y))
  
  for (i in 1:ncol(x)){
    x1<-x[,i]
    y1<-y[,i]
    r<-rstandard(lm(y1~x1))
    out[,i]<-r
  }
  return(out)
}
