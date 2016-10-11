## These functions calculate the invers of a given square matrix.
## The resulting inverted matrix is stored in cache to reduce
## calculation effort. Unless the original matrix is been changed
## the inverted matrix is returned from the cache.
##
## Example:
##
## myMatrix<-makeCacheMatrix(matrix(c(4,1,3,1), nrow=2, ncol=2))
##
## cacheSolve(myMatrix)
##
## [,1] [,2]
## [1,]    1   -3
## [2,]   -1    4


## makeCacheMatrix instantiates the matrix object providing
## four functions (set, get, setinvrs and getinvrs) as well 
## as two data objects (x the original matrix and i the
## inverted matrix).

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


## cacheSolve calculates the inverted matrix for a given
## square matrix.
## The function returns the cached matrix, if already calculated
## and the original matrix not been changed.
## Otherwise the original matrix is been retrieved from cache, 
## the inverted matrix is been calculated using the solve function
## the calculated matrix is been stored in cache and returned.
## The function checks if the original matrix is invertible 
## (determinant != 0) and if the original matrix is a square matrix.

cacheSolve <- function(x, ...) {
  invrs <- x$getinvrs()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  matrx <- x$get()
  if(nrow(matrx) != ncol(matrx)) {
    message("not a square matrix")
  } else if(det(matrx) == 0) {
    message("matrix not invertible")
  } else {
    invrs <- solve(matrx, ...)
    x$setinvrs(invrs)
    invrs
  }
}
