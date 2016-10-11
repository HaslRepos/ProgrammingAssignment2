## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinvrs <- function(invMat) i <<- invMat
  getinvrs <- function() i
  list(set = set, get = get,
       setinvrs = setinvrs,
       getinvrs = getinvrs)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrs <- x$getinvrs()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  matrx <- x$get()
  if(det(matrx) == 0) {
    message("matrix not invertible")
  } else {
    invrs <- solve(matrx, ...)
    x$setinvrs(invrs)
    invrs
  }
}
