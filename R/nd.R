# just a silly wrapper for node depth extraction
# the ape code is ridiculously long so this is for the lazy folks out there

nd<-function(tree, node){
  return(node.depth.edgelength(tree)[node])
}
