# reads BayesTraits .Log file and plots parameterization
# plot is written to file

plotModel = function(info){
  
  nstates = max(info$rPar)
  nrates = nrow(info)

  model = getModel(info)
  parnames = levels(as.factor(model))
  npar_unique = length(parnames)
  
  colopts<-c("#ffa270", "#f4cc66", "#8de8ce", "#5cbcbc", "#b8a9ce", "#8478c9", "#a5a5a5", "#d1d1d1", "#85b267", "#c5e8ae")
  tcols<-colopts[1:npar_unique]
  
  r = nrow(model)
  c = ncol(model)
  
  rows = matrix(rep(1:r, times = c), nrow = r, ncol = c)
  cols = matrix(rep(rev(1:c), times = r), nrow = r, ncol = c, byrow = TRUE)
  c.names = matrix(rep(1:c, times = r), nrow = r, ncol = c, byrow = TRUE)
  
  xmax = vector(mode="numeric", length = length(model))
  xmax[1:length(xmax)] = NA
  
  plotpars = data.frame("xmax" = xmax, "xmin" = xmax, "xmid" = xmax,
                        "ymax" = xmax, "ymin" = xmax, "ymid" = xmax,
                        "col" = xmax, "name" = paste("q", c.names, rows, sep=""))
  
  for (i in 1:length(model)){
    
    xmax = rows[i]
    ymax = cols[i]
    
    xmin = xmax-1
    ymin = ymax-1
    
    xmid = xmin + ((xmax-xmin)/2)
    ymid = ymin + ((ymax-ymin)/2)
    
    parname = model[i]
    
    if (is.na(parname)==TRUE){
      parcol<-"black"
    } else {
      parcol<-tcols[match(parname, parnames)]
    }
    
    plotpars[i,1:(ncol(plotpars)-1)] = c(xmax, xmin, xmid, ymax, ymin, ymid, parcol)
    
  }
  
  plotpars[,1:6] = apply(plotpars[,1:6], 2, as.numeric)
  
  textpars = plotpars[which(plotpars$col != "black"),]
  names = as.character(textpars$name)
  
  plot(NULL, ylim=c(0,nstates), xlim=c(0,nstates), axes = FALSE, xlab=NA, ylab=NA)
  
  rect(plotpars$xmin, plotpars$ymin, plotpars$xmax, plotpars$ymax, col=plotpars$col)
  
  for (i in 1:nrow(textpars)){
    x = textpars$xmid[i]
    y = textpars$ymid[i]
    text = textpars$name[i]

    text(x = x, y = y, labels = text)
        
  }
}
