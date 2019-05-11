# builds on the original bind.tree command from the R package 'ape'
# automatically prunes redundant tips and standardizes branch lengths


treeBind<-function(tree, bindon, slice=NULL){
  if (is.null(slice)==FALSE){
    species.keep<-slice
    spp<-which(bindon$tip.label %in% species.keep == TRUE)
    sp1<-bindon$tip.label[min(spp)]
    sp2<-bindon$tip.label[max(spp)]
    bindon<-extract.clade(bindon, node=getMRCA(bindon, tip=c(sp1, sp2)))
  }

  targ<-getMRCA(tree, tip=c(bindon$tip.label[1],
                      bindon$tip.label[length(bindon$tip.label)]))
  graft<-drop.tip(bindon, tip=bindon$tip.label[1])

  bindon$tip.label[1]<-"drop1"
  grafted.species<-graft$tip.label
  repeatedNames<-tree$tip.label[tree$tip.label %in% graft$tip.label]
  # replace names
  tree$tip.label[which(tree$tip.label %in% repeatedNames)]<-paste("drop", c(2:(length(graft$tip.label))), sep="")

  # bind the tree
  graft.out<-bind.tree(tree, bindon, where=targ)

  # drop the dummynames
  graft.out<-drop.tip(graft.out, tip=paste("drop", c(1:length(graft$tip.label)), sep=""))

  targ.new<-getMRCA(graft.out, tip=c(bindon$tip.label[1], bindon$tip.label[length(bindon$tip.label)]))

  len<-treeLength(graft.out)
  droot<-nd(graft.out)[targ.new]
  d<-len-droot

  for (i in 1:length(grafted.species)){
    tip<-which(graft.out$tip.label==grafted.species[i])

    edge<-which(graft.out$edge[,2]==tip)

    parent.node<-graft.out$edge[edge,1]

    l<-graft.out$edge.length[edge]

    dtip<-nd(graft.out)[tip]
    adj<-len-dtip
    graft.out$edge.length[edge]<-l+adj
  }

  return(graft.out)

}
