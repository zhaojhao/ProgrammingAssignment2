## How to test:
##>  mat <- matrix(runif(9), nrow=3)   # generate a random matrix
##>  cas <- makeCacheMatrix(mat)       # generate cache
##>  cacheSolve(cas)                   # solve matrix from the cache
## 
## The result from the first run should be a matrix which is the inverse of 'mat'.
##
##>  cacheSolve(cas)
##
## The result from the second run should contain "getting cached data" before 
## the inverse of 'mat' returned.
##

## This function creates an object that can cache the inverse of an input matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function retrieve the created inverse of the matrix set up by
# the makeCacheMatrix function. If the inverse has already been calculated
# then the cachesolve should retrieve the inverse from the cache. Otherwise
# solve the inverse of the matrix and store it into the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
