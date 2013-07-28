#' Convert kilometers to miles
#'
#' @export
#' @param km numeric value in kilometers (km)
#' @return numeric a value in miles
km2mi <- function(km) {
  return(km/mi2km(1))
}

#' Convert miles to kilometers
#'
#' @export
#' @param mi numeric value in miles (mi)
#' @return numeric a value in km
mi2km <- function(mi) {
  return(in2m(mi*63360)/1000)
}

#' Convert inches to meters
#'
#' @export
#' @param inches A value in inches
#' @return numeric a value in meters
in2m <- function(inches) {return((inches * 2.54 * .01))} #converting from inches to cm to meters

#' Convert meters to inches
#'
#' @export
#' @param m A value in meters
#' @return numeric a value in inchs
m2in <- function(m) {return(m / in2m(1))}
NULL

#' Calculate the projection position of a real world object
#'
#' Given the position of an object in 3D space and the depth of the projection plane,
#' calculate the position of the projection of that object on projection plane
#'
#' @param TargetPosition The targets' position in width and/or height coordinates
#' @param TargetDepth    The targets' position in depth coordinates
#' @param ScreenDepth    The position of the projection plane in depth coordinates
#' @export conv3Dto2D
#' @return numeric Projection position in the coordinate type specified in TargetPosition
conv3Dto2D <- function(TargetPosition,TargetDepth,ScreenDepth) {
  TargetPosition * (ScreenDepth/TargetDepth)
}
NULL

#' Calculate a real world position of a projected object
#'
#' Given the position of an object on the projection plane, the real depth of the object,
#' and the depth of the projection plane,
#' calculate the position of the real-world object
#'
#' @export conv2Dto3D
#' @param ScreenPosition The position of the object on the screen
#' @param TargetDepth    The targets' position in depth coordinates
#' @param ScreenDepth    The position of the projection plane in depth coordinates
#' @return numeric 3D position in the coordinate axis specified in TargetPosition
conv2Dto3D <- function(ScreenPosition,TargetDepth,ScreenDepth) {
	ScreenPosition/(ScreenDepth/TargetDepth)
}
NULL

#' Calculate the distance between two 3D points
#'
#' @export dist3D
#' @param point1 The location of the first point as a numeric vector of length 3 c(X,Y,Z) or ordered list
#' @param point2 The location of the second point as a numeric vector of length 3 c(X,Y,Z) or ordered list
#' @return numeric distance between two points
#' @references
#' \url{http://www.calculatorsoup.com/calculators/geometry-solids/distance-two-points.php}
#' @examples 
#' dist3D(c(5,6,2),c(-7,11,-13))
#' dist3D(list(c(0,0),c(0:0),c(0,0)),list(1:2,3:4,5:6))
#' dist3D(list(5,6,2),list(-7,11,-13))
dist3D <- function(point1,point2) {
  if(is.list(point1) & is.list(point2)) {
    if ((!is.list(point1)) | (!is.list(point2))) {stop("In russmisc::dist3D: point1 or point2 is not a list")}
    if (!all(names(point1)==names(point2))) {stop("In russmisc::dist3D: List names not the same for point1 and point2")}
    if ((!length(point1)==3) & (!length(point2)==3)) {stop("In rusmisc::dist3D: point1 or point2 has more than 3 list items")}  
    #making sure we have all of the datapoints
    point1.lengths <- unlist(lapply(point1,length))
    point2.lengths <- unlist(lapply(point2,length))
    lengths <- c(point1.lengths,point2.lengths)
    max.length <- max(lengths)
    if(max.length==1) #if the maximum list length is one, then we can process these points like vectors
    {
      return(dist3D(unlist(point1),unlist(point2)))
    }
    #fleshing out the lists if there is just a single entry, I assume that value should be repeated
    if (point1.lengths[1]==1) {point1[[1]] <- rep(point1[[1]],max.length)}
    if (point1.lengths[2]==1) {point1[[2]] <- rep(point1[[2]],max.length)}
    if (point1.lengths[3]==1) {point1[[3]] <- rep(point1[[3]],max.length)}
    if (point2.lengths[1]==1) {point2[[1]] <- rep(point2[[1]],max.length)}
    if (point2.lengths[2]==1) {point2[[2]] <- rep(point2[[2]],max.length)}
    if (point2.lengths[3]==1) {point2[[3]] <- rep(point2[[3]],max.length)}
    #now that we are fleshed out, we check all of the datapoints again
    point1.lengths <- unlist(lapply(point1,length))
    point2.lengths <- unlist(lapply(point2,length))
    lengths <- c(point1.lengths,point2.lengths)
    max.length <- max(lengths)
    #and check to see if we have what we need to continue  
    if (!all(c(point1.lengths==max.length),(point2.lengths==max.length))) {
      stop("In russmisc::dist3d: number of items in the lists are not equal")
    }
    differences <- mapply(function(x,y) {x-y},point1,point2)
    if (!is.vector(differences)) {
      differences.squared <- apply(differences,2,function(x) {x*x})
    } else {
      differences.squared <- differences^2
    }
    differences.squared.summed <- apply(differences.squared,1,sum)
    differences.squared.summed.rooted <- sqrt(differences.squared.summed)
    return(differences.squared.summed.rooted)
  }
  return(sqrt(sum((point2-point1)^2)))
}
NULL

#' Calculate the pythagorean theorem to solve for A, B, or C
#'
#' Given that A and B are the legs of a right triangle and C is its hypotenuse: solve for A, B, or C given any other two.
#'
#' @export
#' @param a Leg 1
#' @param b Leg 2
#' @param c Hypotenuse
#' @return numeric The length of the unspecified side
#' @references
#' \url{http://en.wikipedia.org/wiki/Pythagorean_theorem}
#' @examples
#' pythag(a=3,c=pythag(3,4))
pythag <- function(a = NULL, b = NULL, c = NULL) {
    if ((is.null(a) + is.null(b) + is.null(c)) != 1) {
        stop("In russmisc::pythag: Two, and only two, values must be specified!")
    }
    if (is.null(c)) {
        return(sqrt(a^2 + b^2))
    }
    else {
        #we know we are solving for a leg
        if (is.null(a)) 
            {
                a <- b
            }  #just get it into a standard form
        return(sqrt(c^2 - a^2))
    }
}
NULL

#' Solve right-triangles systems involving (monitor) aspect ratios
#'
#' I designed this to calculate values for monitor aspect ratios, but I imagine it is more general.
#' Given any two or three values, solve for the missing values.  Note, the values are not checked so the principle of GIGO applies.
#'
#' @export
#' @importFrom MASS fractions
#' @param w Width
#' @param h Height
#' @param d Diagonal
#' @param aspect Aspect ratio (w/h)
#' @return numeric The length of the unspecified side
#' @references
#' \url{http://en.wikipedia.org/wiki/Pythagorean_theorem}
#' @examples
#' pythag(a=3,c=pythag(3,4))
aspect <- function(w = NULL, h = NULL, d = NULL, aspect = NULL) {
    have.w <- !is.null(w)
    have.h <- !is.null(h)
    have.d <- !is.null(d)
    have.aspect <- !is.null(aspect)
    if ((have.w + have.h + have.d + have.aspect) > 2) {
        stop("In russmisc::aspect: At least two values must be specified!")
    }
    #we want to get w and h however we can as a standard
    if (have.aspect & have.h & !have.w) {
        w <- h * aspect
        have.w <- TRUE
    }
    if (have.aspect & have.w & !have.h) {
        h <- w/aspect
        have.h <- TRUE
    }
    if (have.aspect & have.d & !have.w) {
        w <- d/pythag(1/aspect, 1) #assuming w is the greater of h and w
        have.w <- TRUE
    }
    if ((have.w + have.h + have.d) == 2) {
        if (!have.w) {
            w <- pythag(NULL, h, d)
            have.w <- TRUE
        }
        if (!have.h) {
            h <- pythag(w, NULL, d)
            have.h <- TRUE
        }
        if (!have.d) {
            d <- pythag(w, h, NULL)
            have.d <- TRUE
        }
    }
    if ((!have.aspect) & (have.h & have.w)) {
        aspect <- w/h
        have.aspect <- TRUE
    }
    if (!all(c(have.w, have.h, have.d, have.aspect))) {
        stop("In russmisc::aspect: A solution was not found!")  
    }
    return(list(w = w, h = h, d = d, aspect = MASS::fractions(aspect)))
} 
NULL

#' Convert degrees to radians
#'
#' @export
#' @param degrees A value in degrees 
#' @return The value provided in degrees converted to radians
#' @examples 
#' radians(90)
radians <- function(degrees)
  {
  constval <- pi/180
  return(degrees*constval)
  }
NULL

#' Convert radians to degrees
#' @export
#' @param radians A value in radians
#' @return The value provided in radians converted to degrees
#' @seealso \code{\link{radians}}
#' @examples 
#' degrees(pi)
degrees <- function(radians)
  {
  constval <- pi/180
  return(radians/constval)
  }
NULL

#' Calculate visual angle given a stimulus size and viewing distance
#' @export
#' @param stimsize.val The stimulus size
#' @param viewdist.val The viewing distance
#' @param offset       The offset of the center of the object from the center of the viewing angle
#' @note The units of measure should be the same for both parameters
#' @return Degrees in visual angle for the given stimulus size
#' @seealso \code{\link{stimsize}} \code{\link{viewdist}} \code{link{visangle3D}}
#' @examples 
#' visangle(10,100)
#' visangle(visangle(0.75438,45),viewdist(visangle(0.75438,45),0.75438))
visangle <- function(stimsize.val,viewdist.val,offset=0) {
  if (offset==0) {
    return(degrees(2*atan(stimsize.val/(2*viewdist.val))))
  } else {
    #get the visual angle from the center to the far edges of the object if it were reflected
    #symmetrically over the center of the viewing position
    bigstim <- (offset-stimsize.val/2+stimsize.val)*2
    bigvisangle <- visangle(bigstim,viewdist.val)
    nonstim <- (offset-stimsize.val/2)*2
    nonstimvisangle <- visangle(nonstim,viewdist.val)
    return(bigvisangle/2-nonstimvisangle/2)
  }
}
NULL

#' Calculate a stimulus size given a visual angle and viewing distance
#' @export
#' @param visangle.val A value in degrees of visual angle
#' @param viewdist.val A viewing distance
#' @return The stimulus size that when centrally presented would have a visual angle (in degrees) of visangle.val.  Stimulus size is reported in the same units as viewdist.val.
#' @seealso \code{\link{viewdist} \link{stimsize}}
#' @examples 
#' stimsize(visangle(10,100),100)
stimsize <- function(visangle.val,viewdist.val) {return(2*(tan(radians(visangle.val/2))*viewdist.val))}
NULL

#' Calculate a viewing distance given a visual angle and stimulus size
#' @export
#' @param visangle.val A value in degrees of visual angle
#' @param stimsize.val A stimulus size
#' @return The viewing distance of an object of stimsize.val that, when centrally presented, would have a visual angle (in degrees) of visangle.val.  Viewing distance is reported in the same units as stimsize.val.
#' @author Russell S. Pierce \email{Russell.S.Pierce@@gmail.com}
#' @seealso \code{\link{viewdist} \link{visangle}}
#' @examples 
#' viewdist(visangle(10,100),10)
viewdist <- function(visangle.val,stimsize.val) {(stimsize.val/2)/tan(radians(visangle.val/2))}
NULL

#' Use the law of sines 
#' Use the law of cosines to calculate an angle
#'
#' Given that A, B, and C are the legs of a triangle solve the the opposite angles
#'
#' @export
#' @param A numeric Length of side A
#' @param B numeric Length of side B
#' @param C numeric Length of side C
#' @return list(a=numeric angle opposite A,b=numeric angle opposite B,c=numeric angle opposite C)
#' @references
#' \url{http://oakroadsystems.com/twt/solving.htm#eq30}
#' @examples
#' lawOfCos(3,4,5)
#' lawOfCos(3,3,3)
#' stimsize.val <- 10 #the stimulus size
#' dist <- 15 #the distance
#' visangle(10,15)-lawOfCos(stimsize.val,pythag(stimsize.val/2,dist),pythag(stimsize.val/2,dist))$a
#' offset <- 5
#' visangle(10,15,5)-lawOfCos(stimsize.val,pythag(stimsize.val/2+offset,dist),pythag(stimsize.val/2-offset,dist))$a
lawOfCos <- function(A,B,C) {
  a <- (B^2  + C^2 - A^2) / (2*B*C)
  b <- (A^2  + C^2 - B^2) / (2*A*C)
  c.tmp <- (A^2  + B^2 - C^2) / (2*A*B)
  biggerthanpi <- c(c(a,b,c.tmp) > pi)
  if (any(biggerthanpi,na.rm=TRUE)) {
    stop("In russmisc::lawOfCos: The side lengths provided do not form a regular triangle")
  }
  return(list(
    a=degrees(acos(a)), #find A
    b=degrees(acos(b)), #find B
    c=degrees(acos(c.tmp)) #find C
  ))
}
NULL

#' Calculate visual angle given a 3D viewer position, stimulus size, and stimulus position
#'
#' 
#' This function calculates the visual angle of a stimulus given a viewer position, stimulus size, and stimulus position.
#' Here I make fewer assumptions and calculate the distance between the viewer and the edges of the object in order to 
#' use the law of cosines to calculate the associated angle.
#'
#' @export
#' @param viewerloc The location of the center of the viewer as a numeric vector of length 3 c(X,Y,Z)
#' @param stimloc The location of the center of the stimulus as a numeric vector of length 3 c(X,Y,Z)
#' @param stimdim The stimulus dimensions as a numeric vector of length 2 c(width,height)
#' @note The units of measure should be the same for all parameters.  Z is assumed to be the depth plane.
#' @return Degrees in visual angle for the given stimulus dimentions and positions of viewer and stimuli
#' @seealso \code{\link{visangle}}
#' @examples 
#' #corresponds to an offset of 10 and 10 and a viewer distance of 20
#' viewerloc <- c(0,0,0)
#' stimloc<-c(10,10,-20) 
#' stimsize.val <- 10
#' stimdim <- c(stimsize.val,1)
#' visangle(stimsize.val,20,pythag(stimloc[1],stimloc[2]))
#' visangle3D(viewerloc,stimloc,stimdim)
#' #corresponds to an offset of 10 and 0 and a viewer distance of 20
#' viewerloc <- c(0,0,0)
#' stimloc<-c(10,0,-20) 
#' stimsize.val <- 10
#' stimdim <- c(stimsize.val,1)
#' visangle(stimsize.val,20,pythag(stimloc[1],stimloc[2]))
#' visangle3D(viewerloc,stimloc,stimdim)
#' visangle3D(list(c(1,1),c(1,1),c(1,1)),list(c(2,2),c(2,2),c(2,2)),list(c(3,3),c(3,3)))
#' visangle3D(list(1,2,3),list(3,4,5),list(2,2))

visangle3D <- function(viewerloc,stimloc,stimdim) {
  if(all(sapply(list(viewerloc,stimloc,stimdim),is.list))) {
    return(visangle3D.l(viewerloc,stimloc,stimdim))
  }
  
  stim.left.pos <- c(stimloc[1]-stimdim[1]/2,stimloc[2],stimloc[3])
  stim.right.pos <- c(stimloc[1]+stimdim[1]/2,stimloc[2],stimloc[3])
  A <- dist3D(stim.left.pos,viewerloc)
  C <- dist3D(stim.right.pos,viewerloc)
  B <- stimdim[1]
  XaxisAngle <- lawOfCos(A,B,C)$b
  stim.top.pos <- c(stimloc[1],stimloc[2]-stimdim[2]/2,stimloc[3])
  stim.bottom.pos <- c(stimloc[1],stimloc[2]+stimdim[2]/2,stimloc[3])
  A <- dist3D(stim.top.pos,viewerloc)
  C <- dist3D(stim.bottom.pos,viewerloc)
  B <- stimdim[2]
  YaxisAngle <- lawOfCos(A,B,C)$b
  return(list(VisualAngle.X=XaxisAngle,VisualAngle.Y=YaxisAngle))
}
NULL

#' Helper function for visangle3D to handle lists
#'
#' @param viewerloc The location of the center of the viewer as a list
#' @param stimloc The location of the center of the stimulus as a list
#' @param stimdim The stimulus dimensions as a numeric list
visangle3D.l <- function(viewerloc,stimloc,stimdim) {
  names(viewerloc) <- c("X","Y","Z")
  names(stimloc) <- c("X","Y","Z")
  names(stimdim) <- c("X","Y")
  viewerloc.x <- viewerloc$X
  viewerloc.y <- viewerloc$Y
  viewerloc.z <- viewerloc$Z
  stimloc.x <- stimloc$X
  stimloc.y <- stimloc$Y
  stimloc.z <- stimloc$Z
  stimdim.x <- stimdim$X
  stimdim.y <- stimdim$Y
  if (all(sapply(list(viewerloc,stimloc,stimdim),length)==c(3,3,2))) {
    #turf the processing back to the vector version of visangle3D if there is only a single set of points
    visangle3D(unlist(viewerloc),unlist(stimloc),unlist(stimdim))
  }
  stim.left.pos <- list(X=stimloc.x-stimdim.x/2,Y=stimloc.y,Z=stimloc.z)
  stim.right.pos <- list(X=stimloc.x+stimdim.x/2,Y=stimloc.y,Z=stimloc.z)
  A <- dist3D(stim.left.pos,viewerloc)
  C <- dist3D(stim.right.pos,viewerloc)
  B <- stimdim.x
  XaxisAngle <- lawOfCos(A,B,C)$b
  stim.top.pos <- list(X=stimloc.x,Y=stimloc.y-stimdim.y/2,Z=stimloc.z)
  stim.bottom.pos <- list(X=stimloc.x,Y=stimloc.y+stimdim.y/2,Z=stimloc.z)
  A <- dist3D(stim.top.pos,viewerloc)
  C <- dist3D(stim.bottom.pos,viewerloc)
  B <- stimdim.y
  YaxisAngle <- lawOfCos(A,B,C)$b
  return(list(VisualAngle.X=XaxisAngle,VisualAngle.Y=YaxisAngle))
}
NULL