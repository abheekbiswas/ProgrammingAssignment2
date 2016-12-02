## A pair of functions to cache the inversion of a matrix
## functions do

## Function 1: makeCacheMatrix creates an object of class: matrix which can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverse) invm <<- inverse
  getInverseMatrix <- function() invm
  list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## Function 2: cacheSolve calculates the inverse of the matrix returned by Function 1: makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invm <- x$getInverseMatrix()
  if (!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  mat <- x$get()
  invm <- solve(mat, ...)
  x$setInverseMatrix(invm)
  invm
}