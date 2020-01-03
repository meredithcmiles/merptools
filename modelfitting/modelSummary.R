modelSummary <- function(modelset, effect = NULL){
  
  effect.supplied <- !is.null(effect)
  multi <- FALSE
  model <- anova(modelset[[1]])
  aics <- AIC(modelset[[1]])
  
  
  if(effect.supplied && is.list(effect)){
    effect <- unlist(lapply, effect, deparse)
    
  } else if (!effect.supplied){
    x <- rownames(anova(modelset[[1]]))
    effect <- x[!"(Intercept)" == x]
    
    if(length(effect) > 1) multi <- TRUE
  }
  
  modelnames <- names(modelset)
  nmodels <- length(modelset)
  
  columns <- c("numDF","denDF","F", "p")
  
    column.nms <- paste(effect[1], columns, sep=".")
    
    if(multi){
      # code for multiple entries only
      nFX <- length(effect)
      
      for(i in 2:(nFX)){
        column.nms <- c(column.nms, paste(effect[i], columns, sep="."))
      }
    }

  
  n.col <- length(column.nms)
  
  output <- matrix(NA, nrow = nmodels, ncol = n.col, dimnames = list(modelnames, column.nms))
    
    # code for single AND multiple entries
    target.row <- which(rownames(model) %in% effect)
    
    for(i in 1:nmodels){    
      
      if(i > 1) {
        model <- anova(modelset[[i]])
        aics <- c(aics, AIC(modelset[[i]]))
      }
      
    if(multi){
      output[i,] <- as.vector(t(model[target.row,]))
      
    } else {
      output[i,] <- unlist(model[target.row,])[1:4]
       }
    }
  
  topmodel <- which(aics == min(aics))
  dAIC <- aics - min(aics)
  
  rownames(output)[topmodel] <- paste(rownames(output)[topmodel], "*", sep="")
    
  return(cbind("AIC" = aics, "dAIC" = dAIC, output))
    
}
