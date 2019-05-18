# detect and extract parameter intro from BayesTraits MCMC .Log files

getInfo<-function(file, states=NULL){
  # raw output for the MCMC starts on line 25
  skp<-25
  # we'll use line length as a clue to find where to read
  tmp<-readLines(file, n=100)
  tmplen<-str_length(tmp)
  # the last line of rate definitions occurs right before
  # the only line with exactly 18 characters in it
  end<-which(tmplen==18)-1
  n = end - skp
  
  # now we can read only the relevant information
  readin<-read.table(file, skip=25, header=FALSE,
                     nrows = n, col.names=c("par", "val"),
                     colClasses=c("character", "character"))
  
  # replace free parameters with their id
  readin$val[which(readin$val=="None")] <- readin$par[which(readin$val=="None")]
  # encode zero parameters as missing data
  readin$val[which(readin$val=="0.000000")]<-NA
  
  # we'll store some data for easier manipulation later
  x<-as.numeric(substr(readin$par, 2, 2))   # adj matrix row index / parameter name part 1
  y<-as.numeric(substr(readin$par, 3, 3))   # adj matrix col index / parameter name part 2
  x1<-as.numeric(substr(readin$val, 2, 2)) # parameter definition pt. 1
  x2<-as.numeric(substr(readin$val, 3, 3)) # parameter definition pt. 2
  
  npar <- length(x)
  
  nstate<-max(as.numeric(x))
  nrate<-(npar^2)-npar
  
  out<-data.frame(rPar=x, cPar=y, 
                  rVal=x1, cVal=x2)

  return(out)
  
}
