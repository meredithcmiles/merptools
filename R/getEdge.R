# simple wrapper for ape data extraction
# returns the edge formed between two nodes
# can either be two internal nodes, or internal + external(tip)

getEdge<-function(tree, node1, node2){
  edge<-which(tree$edge[,1]==node1 && tree$edge[,2]==node2)
  return(edge)
}
