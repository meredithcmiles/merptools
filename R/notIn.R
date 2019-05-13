notIn<-function(x, y){
  x_not_y<-x[which(x %in% y == FALSE)]
  y_not_y<-y[which(y %in% x == FALSE)]
  return(list("x_not_y"=x_not_y, "y_not_x"=y_not_x))
}
