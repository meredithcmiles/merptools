require(btw)

runbt = function(data, tree, files, models){
  
  options(stringsAsFactors = FALSE)
  
  for (j in 1:length(models)){  # enter for-loop
    
    cmd = readLines(files[j]) # read command file for first model
    vec.out = vector(mode = "numeric", length = ncol(data)) # prepare vector
    
    for (i in 1:ncol(data)){
      
      dat = as.data.frame(cbind(rownames(data), data[,i]))
      
      if(i==1){
        
        lh = bayestraits(dat, tree, cmd[-4], remove_files = FALSE)$Log$results$Lh
        vec.out = lh
        
      } else if(i < ncol(data) & j < length(models)){
      
        lh = bayestraits(dat, tree, cmd[-4], remove_files = FALSE)$Log$results$Lh
        vec.out = c(vec.out, lh)
        
      } else {
        
        lh = bayestraits(dat, tree, cmd[-4], remove_files = TRUE)$Log$results$Lh
        vec.out = c(vec.out, lh)
      }
      
      if (j==1){
        
      l1 = paste("Model ", j, ", Iteration ", i, sep="")
      l2 = paste("Running Lh0 = ", mean(vec.out), sep="")

      cat(l1, "\n", l2, "\n")
      
      } else {
       
        if (i == 1){
          
          x2 = 2*(vec.out[i]-out[i,1])
          
          l1 = paste("Model ", j, ", Iteration ", i, sep="")
          l2 = paste("Running x2 = ", x2, sep="")
          
          cat(l1, "\n", l2, "\n")
          
        } else {
          
          x2 = c(x2, 2*(vec.out[i] - out[i,1]))
          l1 = paste("Model ", j, ", Iteration ", i, sep="")
          l2 = paste("Running X2 = ", mean(x2),sep="")
          
          cat(l1, "\n", l2, "\n")
                      
        }
        
      }
      
    }
    
    if (j==1){
      
      out = as.data.frame(vec.out)
      colnames(out)[1] = models[j]
      
    } else {
      
      out = cbind(out, vec.out)
      colnames(out)[j] = models[j]
      
      if (j==2){
        
        x2.out = as.data.frame(x2)
        colnames(x2.out)[1] = models[j]
        
      } else {
        
        x2.out = cbind(x2.out, x2)
        colnames(x2.out)[j] = models[j]
        
      }
      
    }
    
  }
  
  return(list("Lh" = out, "x2" = x2.out))
  
}
