# function to summon a blank plot
blankplot <- function(ylim=c(-10,10), xlim=c(-10,10)){
  plot(NULL, ylim=ylim, xlim=xlim,
       axes = FALSE, xlab=NA, ylab=NA)
}

# function to draw a triangle
triangle <- function(angles=NULL, sides=NULL, unit=NULL, orient = NULL,
                     invert = c(FALSE, FALSE), lims=NULL, plot=TRUE, add=FALSE, dat=TRUE, ...){
  
  error <- FALSE
  
  # check for axis limit argument
  if(is.null(lims)){
    autolims <- TRUE
  }
  
  # default orientation
  if(is.null(orient)){
    orient <- 1
  }
  
  # default unit is radians
  if(is.null(unit)){
    degrees <- FALSE
  }
  
  # check if user supplied correct sides arguments
  
  if(!is.null(sides)){
    
    if(length(sides) > 3){
      # a triangle can only have 3 sides...
      stop('No more than 3 sides can be specified.')
      
    } else if (length(sides) == 3) {
      sides <- sides[order(sides)]
      mode <- "sides"
      
    } else if(length(sides) == 2){
      if(is.null(angles)){
        mode <- "hypotenuse"
      } else {
        mode <- "combo"
      }
    } else {
      error <- TRUE
    }
  }
  
# temporary fix to plot combo as sides for now:
  
if(error){
  stop('Side methods supported right now, please specify 2 or more sides.')
}
  
  
# methods definition for drawing from sides:
  
if(mode %in% c("sides", "hypotenuse")){
  
  if(mode=="hypotenuse"){
    sides <- c(sides, sqrt(sides[1]^2 + sides[2]^2))
  }
  
  if(sides[1] + sides[2] <= sides[3]){
    warning('Sum of 2 short sides must be GREATER than the longest side')
  }
  
 if (orient == 2){
    # this argument flips the x and y side
    sides <- sides[c(2,1,3)]
  }
  
  if(autolims){
    # set axis limits for plotting
    lims.x <- c(0, sides[1] + 0.15*sides[1])
    lims.y <- c(0, sides[2] + 0.15*sides[2])
  } else {
    # or otherwise, use the user-supplied limits
    lims.x <- lims[[1]]
    lims.y <- lims[[2]]
  }
  
  # flip the triangle on one or both axes
  if(TRUE %in% invert){
    
    sides[1:2][invert] <- sides[1:2][invert] * -1
    
    if(invert[1]){
      lims.x <- lims.x * -1
    }
    
    if(invert[2]){
      lims.y <- lims.y * -1
    }
    
  }
  
  side.x <- c(0,0,sides[1],0)
  
  if(mode=="hypotenuse"){ 
    side.y <- c(0,0,0,sides[2])
    side.z <- c(side.x[3:4], side.y[3:4])
  } else if (mode=="sides"){
    y0 <- side.x[1:2]
    z0 <- side.x[3:4]
    
    a <- sides[1]
    b <- sides[2]
    c <- sides[3]
    
    alpha <- acos((b^2 + c^2 - a^2)/(2*b*c))
    h <- a*cos(alpha)
    x1 <- a*sin(alpha)
    
    side.y <- c(y0, x1, h)
    side.z <- c(x1, h, z0)
    
    }
  }
  
  Psides <- list("x" = side.x, "y" = side.y, "z" = side.z)
  
    if(!add){
      blankplot(xlim = lims.x, ylim = lims.y)
    }
    
  if(plot){
    for (i in 1:3){
      p <- Psides[[i]]
      segments(p[1], p[2], p[3], p[4])
    }
  }
  
  if(dat){
    return(Psides)
  }
  
}
