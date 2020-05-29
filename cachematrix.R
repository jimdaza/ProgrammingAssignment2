## A pair of functions that cache the inverse of a matrix
## to avoid the time-costly recalculations

## This function creates a matrix that "caches" its inverse

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  set <- function(y){
    x <<- y
    invmatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) invmatrix <<- solveMatrix
  getInverse <- function() invmatrix
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function shows the inverted matrix created previously and,
## if the matrix remains unchanged, shows the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmatrix <- x$getInverse()
  if (!is.null(invmatrix)) {
    message("getting cached data")
    return(invmatrix)
  }
  m <- x$get()
  invmatrix <- solve(m, ...)
  x$setInverse(invmatrix)
  invmatrix
}
