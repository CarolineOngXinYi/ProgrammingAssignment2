## a pair of functions that inverse a matrix and
## cache the inverse of a matrix

## This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 i <- NULL
  set <- function(y) { ## set to assign new matrix 
    x <<- y
    i <<- NULL  ## inverse matrix of new matrix reset to NULL
  }
  get <- function() x  ## get function return x matrix
  setinv <- function(solve) i <<- solve
  getinv <- function() i ## get or returns inverse matrix
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
## above makeCacheMatirx function. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) { ## if inverse has previously been called, get inverse from cache
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...) ## inversion happens here
  x$setinv(i) ## set to assign inversion, after this only can run getinv
  i
}
