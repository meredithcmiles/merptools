# uses k-means clustering to determine a salient binning scheme to partition species by their continuous trait value
# requires bootstrapped data as input

binarize<-function(x){
  clustmat<-matrix(NA, nrow=100, ncol=4)
  clusters<-matrix(NA, nrow=nrow(x), ncol=100)
  rownames(clusters)<-rownames(x)
  
  for (i in 1:100){
    dat<-x[,i]
    dat<-dat[complete.cases(dat)]
    ind<-which(rownames(x) %in% names(dat)==TRUE)
    
    clust<-kmeans(dat, 2)
    
    if (clust$centers[1]>clust$centers[2]){
      which1<-which(clust$cluster==1)
      which2<-which(clust$cluster==2)
      
      clust$centers<-clust$centers[c(2,1)]
      clust$withinss<-clust$withinss[c(2,1)]
      
      clust$cluster[which1]<-2
      clust$cluster[which2]<-1
    }
    
    clustmat[i,1:2]<-clust$centers
    clustmat[i, 3:4]<-clust$withinss
    clusters[ind,i]<-clust$cluster
    
  }
  return(list("summary"=clustmat, "clustering"=clusters, "trait.avg"=bootstrapMeans(x)))
}
