## Below are two functions that are used to create a special object that stores 
## a matrix and cache's its inversion.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
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
        ## Return a matrix that is the inverse of 'x'
  im <- x$getInvertedMatrix()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  m <- x$get()
  im <- solve(m, ...)
  x$setInvertedMatrix(im)
  im
}
