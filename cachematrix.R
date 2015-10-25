## Below are two functions that are used to create a special object that stores 
## a matrix and cache's its inversion.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL ## cached inverted matrix
  set <- function(y) {
    x <<- y
    im <<- NULL ## reset cached inverted matrix
  }
  get <- function() x
  setInvertedMatrix <- function(invertedMatrix) im <<- invertedMatrix
  getInvertedMatrix <- function() im
  list(set = set, get = get,
       setInvertedMatrix = setInvertedMatrix,
       getInvertedMatrix = getInvertedMatrix)
}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  im <- x$getInvertedMatrix()
  if(!is.null(im)) { ## cache is filled, return value
    message("getting cached data")
    return(im)
  }
  ## cache is empty, calculate inverse and store in cache
  m <- x$get()
  im <- solve(m, ...)
  x$setInvertedMatrix(im)
  im
}
