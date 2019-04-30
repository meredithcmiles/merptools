# utility function to supply named vectors of comparative data
# use for functions (e.g. phylANOVA, aov.phylo, ace) that are not compatible with vectors extracted from named data.frames
require(ape)

pruneTo<-function(x, template){
  if (is.phylo(x)==TRUE){
    drop<-x$tip.label[!x$tip.label %in% names(template)]
    x<-drop.tip(x, drop)
  }
  else { # if x is a vector to be pruned to data...
    x<-x[!which(is.na(template))]
  }
  return(x)
}
