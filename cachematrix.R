## This R file consists of two functions that are used to create a new CacheMatrix object
## and to calculate the inverse of the invertible square matrix input as needed (will 
## retrieve from cache if already calculated before). This is a proof of concept of using
## special constructs to cache data that may be time consuming to generate

## Author: On Tsang



## makeCacheMatrix : This function will create a CacheMatrix object that consists of get,
## set, getInverse and setInverse sub-functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) m <<- inv
  getInverse <- function() m
  list(setFcn = set, getFcn = get, setInvFcn = setInverse, getInvFcn = getInverse)
}


## cacheSolve : This function will call the 'solve' method to calculate the inverse
## of the invertible square matrix. Retrieve from stored memory if already calculated
## from previous call

cacheSolve <- function(x, ...) {
  m <- x$getInvFcn()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getFcn()
  m <- solve(data, ...)
  x$setInvFcn(m)
  m
}






## Testing
# z <- matrix(c(4, 3, 3, 2), nrow = 2, ncol = 2)
# zz <- makeCacheMatrix(z)
# cacheSolve(zz)
#
# Input :      [,1] [,2]
#         [1,]    4    3
#         [2,]    3    2
#
# Output:      [,1] [,2]
#         [1,]   -2    3
#         [2,]    3   -4