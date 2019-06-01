require(btw)

runbt = function(data, tree, files, modelnames){
  
  models = substr(files, start=1, stop = 3)
  
  for (j in 1:length(models)){
    
    cmd = readLines(files[j])
    vec.out = vector(mode = "numeric", length = 1000)
    
    for (i in 1:ncol(data)){
    
      dat = as.data.frame(cbind(rownames(data), data[,i]))
      vec.out[i] = bayestraits(dat, tree, cmd[-4])$Log$results$Lh
      print(paste(j,i, sep="-"))
      
    }
    
    if (j==1){
    
      out = as.data.frame(vec.out)
      colnames(out)[1] = models[j]
      
    } else {
    
      out = cbind(out, vec.out)
      colnames(out)[j] = models[j]
      
    }
    
  }
  
  return(out)
  
}
