# for perturbation analysis or to discretize continuous traits with k-means clustering

bootstrapTrait<-function(trait, dataframe, n){
  
  taxa<-levels(as.factor(dataframe$species))
  samples<-matrix(NA, nrow=length(taxa), ncol=n)
  rownames(samples)<-taxa
  
  trait<-trait[!is.na(trait)]
  dataframe<-dataframe[!is.na(trait),]
  
  for (i in 1:length(taxa)){
    
    if (taxa[i] %in% names(trait) == TRUE){
      
      rec<-which(names(trait)==taxa[i])
      subdata<-trait[rec]
      
      for (j in 1:n){
        
        samp<-sample(subdata, size=3, replace=TRUE)
        samples[i,j]<-mean(samp)
        
      }
    }
  }
