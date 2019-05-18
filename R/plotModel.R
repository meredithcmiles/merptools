# reads BayesTraits .Log file and plots parameterization
# plot is written to file

plotModel<-function(x, plotname=NULL, format=".svg"){
  
  if (is.matrix(x)==TRUE){
    stop("Just supply the .Log file or info file. I'll call the getModelDesign function.")
  } else if (is.data.frame(x)==TRUE) {
    info <- x
  } else if (is.character(x)==TRUE) {
    info <- getInfo(x)
  }
  
  if (is.null(plotname)==TRUE){
    plotname<-paste("modelPlot","-", sample(1:10000, size=1), "_", Sys.Date(), format, sep="")
  } else {
    plotname <- paste(plotname,"_", Sys.Date(), format, sep="")
  }
  
  nstates<-max(info$rPar)
  nrates<-nrow(info)
  npar<-(nstates^2) - nstates
  parnames<-levels(as.factor(model))
  
  model<-getModelDesign(x)
  
  colopts<-c("#ffa270", "#f4cc66", "#8de8ce", "#5cbcbc", "#b8a9ce", "#8478c9", "#a5a5a5", "#d1d1d1", "#85b267", "#c5e8ae")
  cols<-colopts[1:npar]
  
  rcol<-rev(info$cPar)
  
  svg(plotname, width=5, height=5, pointsize=4)
  
  plot(NULL, ylim=c(0,nstates), xlim=c(0,nstates), axes = FALSE, xlab=NA, ylab=NA)
  
  diag.xmin<-c(0:(nstates-1))
  diag.ymin<-rev(diag.xmin)
  
  diag.xmax<-c(1:nstates)
  diag.ymax<-rev(diag.xmax)
  
  rect(diag.xmin, diag.ymin, diag.xmax, diag.ymax, col="black")
  
  
  for (i in 1:nrates){
    rowind<-info$rPar[i]
    colind<-rcol[i]
    parname<-model[i]
    
    if (is.na(parname)==TRUE){
      parcol<-"black"
    } else {
      parcol<-cols[match(parname, parnames)]
    }
    
    xmax <- rowind
    ymax <- colind
    
    xmin <- rowind-1
    ymin <- colind-1
    
    xmid <- (xmax - xmin)/2
    ymid <- (ymax - ymin)/2
    
    rect(xmin, ymin, xmax, ymax, col=parcol)
    
    
    
  }
  
  dev.off()
  
}
