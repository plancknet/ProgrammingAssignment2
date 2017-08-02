## These functions introduce a special matrix object which caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInversion <- function(solve) i <<- solve
  getInversion <- function() i
  list(set = set, get = get, setInversion = setInversion, getInversion = getInversion)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  i <- x$getInversion()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInversion(i)
  i
}
