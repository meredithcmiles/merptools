# quick function to define axis limits for plotting loops
getlims <- function(x, min=NULL, max=NULL){

  doMin <- is.null(min)
  doMax <- is.null(max)

  multi <- (is.list(x) & length(x) > 1) | ncol(x) > 1

  if(multi & (!is.list(x) | ncol(x) > 1)){
      x <- apply(x, 2, list)
      x <- lapply(x, unlist)
  }


  if(multi){
    out <- vector(mode="list", length=length(x))
  }


  # enter loop
  for (i in 1:length(x)){

    if (multi) {
      dat <- x[[i]]
    } else {
      dat <- x
    }

    if(doMax){
      Max <- ceiling(max(dat, na.rm=T)/10)*10
    } else {
      Max <- max
    }

    if(doMin){
      Min <- ceiling(min(dat, na.rm=T)/10)*10
    } else {
      Min <- min
    }

    lims.out <- c(Min, Max)

    if(multi){
      out[[i]] <- lims.out
    } else {
      out <- lims.out
    }

  }

  return(out)
}
