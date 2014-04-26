##########
# Description : Matrix inversion is usually a time-consuming computation, so
#               in this file there are two functions to caching its inverse, 
#               in order to avoid recomputing when possible.
#
#               The first function "makeCacheMatrix" creates a special "matrix" 
#               object that can cache its inverse. While the second function
#               "cacheSolve" retrieves the inverse of the special "matrix" 
#               from the cache if it has already been calculated (and the matrix 
#               has not changed), otherwise it calculates the inverse matrix and 
#               sets the inverse in the cache.
#
# Author      : Stagirite (Pseudonym used in the github account).
# Created     : 23-04-2014 03:34:30 PM CST
##########

makeCacheMatrix <- function(x = matrix()) {
  # Creates a special "matrix", which is really a list containing a set of
  # functions to:
  # 1.  set the the matrix
  # 2.  get the the matrix
  # 3.  set the inverse of the matrix
  # 4.  get the inverse of the matrix
  #
  # Args:
  #   x   : A matrix.
  #
  # Returns:
  #   A special "matrix" which is an object that allows to cache the inverse
  #   of the matrix given in the "x" argument.

  # We store the inverse matrix in the environment of the function using
  # the local variable "localinversematrix". When we create the special 
  # "matrix" object we intialize the inverse matrix data.
  localinversematrix <- NULL
  
  # Functions to set/get the matrix. 
  setmatrix <- function(matrixtobeassigned) {
    # Using the "<<-" operator we redefine the local variables of the 
    # "makeCacheMatrix" function ("x" and "localinversematrix") in its
    # environment.
    x <<- matrixtobeassigned
    #   ... we initialize the "localinversematrix" variable every time we 
    #       set the matrix in order to force the calculation of the inverse 
    #       when it changes.
    localinversematrix <<- NULL
  }
  getmatrix <- function() x
  
  # Functions to set/get the inverse of the matrix.
  #   Again, we use "<<-" operator to redefine the local variable
  #   "localinversematrix".
  setinversematrix <- function(inverse) localinversematrix <<- inverse
  getinversematrix <- function() localinversematrix
  
  # We store the set/get functions in a list and return it.
  specialmatrix <- list(setmatrix = setmatrix, 
                        getmatrix = getmatrix,
                        setinversematrix = setinversematrix, 
                        getinversematrix = getinversematrix)
  return(specialmatrix)
}

cacheSolve <- function(x, ...) {
  # Calculates the inverse of the "special" matrix "x". It first checks to
  # see if the inverse has already been calculated. If so, it gets the inverse
  # from the cache and skips the computation. Otherwise, it calculates the 
  # inverse of the matrix using the "solve" function defined in the base
  # package and sets the value of the inverse in the cache via the 
  # "setinversematrix" function.
  #
  # Args:
  #   x   : Special "matrix" object created by the "makeCacheMatrix" function 
  #         (defined above).
  #   ... : Arguments to be passed to the "solve" function. See help (?solve).
  #
  # Returns:
  #   The inverse of the matrix contained in the "x" object.  In order to
  #   return the inverse, the matrix should be invertible.
  
  # Query for the "x"'s cache that stores the inverse matrix.
  inversematrix <- x$getinverse()
  
  # If there's a cache, it returns the cache, no computation is needed.
  if(!is.null(inversematrix)) {
    # Report to the user that it's using the cache.
    message("Getting cached data...")
    return(inversematrix)
  }
  
  # If there's no cache it gets the matrix, calculate and store the
  # inverse matrix in the cache and return it.
  matrixdata <- x$getmatrix()
  inversematrix <- solve(matrixdata, ...)
  # Report to the user that the inverse is cached.
  message("Caching the data...")
  x$setinversematrix(inversematrix)
  return(inversematrix)
}
