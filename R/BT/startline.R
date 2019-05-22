startline = function(file){
  # raw output for the MCMC starts on line 25
  skp<-25
  # we'll use line length as a clue to find where to read
  tmp<-readLines(file, n=100)
  tmplen<-str_length(tmp)
  out = min(which(tmplen>100))
  return(out)
}
