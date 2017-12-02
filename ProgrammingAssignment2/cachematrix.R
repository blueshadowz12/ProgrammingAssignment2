## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly 
##(there are also alternatives to matrix inversion 
## that we will not discuss here). 
## Your assignment is to write a pair of functions 
## that cache the inverse of a matrix.

## It creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inn <- NULL
  set <- function(y) {
    x  <<-  y
    inn <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inn <<- inverse
  getInverse <- function() inn
  list( set= set, 
        get =get,
        setInverse = setInverse,
        getInverse = getInverse)
  
}


## It computes the inverse of matrix created by makeCacheMatrix.
## If inverse is already calculated (and matrix did not change),
## it must retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inn <- x$getInverse()
  if(!is.null(inn)) {
    message("getting cached data")
    return(inn)
  }	
  inverted <- x$get()
  inn <- solve(inverted, ...)
  x$setInverse(inn)
  inn
  
}
