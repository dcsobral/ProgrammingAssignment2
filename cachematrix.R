## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creates a list of functions that store and
## retrieve a matrix and its optional inverse.
## This list stands for a "special" matrix
## with caching inverse.
makeCacheMatrix <- function(x = matrix()) {
  # This is going to be the cached inverse
  i <- NULL
  
  # Functions that will be returned
  setM <- function(y) {
    x <<- y
    # When setting a new matrix, "uncache" the inverse
    # of the old one
    i <<- NULL
  }
  setI <- function(y) i <<- y
  getM <- function() x
  getI <- function() i
  
  # Return the list of functions
  list(get = getM, set = setM, 
       inverse = getI, setI = setI)
}


## Write a short comment describing this function

## Given a "special" matrix created by makeCacheMatrix,
## compute the inverse or retrieve an existing cached
## version.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if (is.null(x$inverse())) {
    # Compute the inverse and store it if the
    # inverse is not cached
    x$setI(solve(x$get()))
  }
  
  x$inverse()
}
