# simple wrapper for ape data extraction
# returns the edge formed between two nodes
# can either be two internal nodes, or internal + external(tip)

getEdge<-function(tree, node1, node2){
  hasnode1<-which(tree$edge[,1]==node1)
  hasnode2<-which(tree$edge[,2]==node2)
  edge<-hasnode1[which(hasnode1 %in% hasnode2)]
  return(edge)
}
