# returns the length of a phylogeny in branch-length units

require(ape)

treeLength<-function(tree){
  nTips<-length(tree$tip.label)
  node1.root<-nTips+1
  edgelength.max<-max(tree$edge.length)
  maxedge<-tree$edge[which(tree$edge.length==edgelength.max),]
  
  if (maxedge[2]>node1.root){
    out<-edgelength.max
  } else {
    istip<-tree$edge[,2]<nTips
    target<-which(istip==TRUE)[1]
    out<-sum(tree$edge.length[1:target])
  }
  return(out)
}
