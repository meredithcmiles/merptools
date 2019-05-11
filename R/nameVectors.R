# for instances where pruneTo() is not practical, use this instead
# supply your named data.frame and the columns you'd like to extract
# simply returns a named list of vectors for later handling

require(ape)

nameVectors<-function(data, vectors){
  n<-length(vectors)
  names<-colnames(data[,vectors])
  output<-vector(mode="list", length=n)
  names(output)<-names
  for (i in 1:n){
    vector.out<-data[,vectors[i]]
    names(vector.out)<-rownames(data)
    vector.out<-vector.out[!is.na(vector.out)]
    output[[i]]<-vector.out
  }
  return(output)
}
