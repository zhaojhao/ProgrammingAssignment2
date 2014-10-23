#' Building a pair of functions that cache the inverse of a matrix. 
#' @description 
#' Matrix inversion is usually a costly computation and their may be some benefit 
#' to caching the inverse of a matrix rather than compute it repeatedly. 
#'
#' This function (\code{makeCacheMatrix}) creates a special "matrix" object 'y'
#' that can cache its inverse from the input matrix, 'x'.
#' @param x a large matrix of interest for calculation of its inverse
#' @return y a list object that can cache the inverse of the input matrix
#' @author Zhao Hao
#' @details
#' This function creates an object that holds the input matrix 'x', and setter
#' and getter functions to manage the two matrice. The \code{setinverse} function
#' stores the inverse of the matrix obtained from the \code{cacheSolve} function.
#' The \code{getinverse} function retrieves the inverse of the matrix from the cache, 'm'.
#' @references https://class.coursera.org/rprog-008/human_grading/view/courses/972581/assessments/3/submissions
#' @examples
#'  mat <- matrix(runif(9), nrow=3)   # generate a random matrix
#'  cas <- makeCacheMatrix(mat)       # generate cache
#'  cacheSolve(cas)                   # solve matrix from the cache
#'  ## The result from the first run should be a matrix which is the inverse of 'mat'.
#'  cacheSolve(cas) 
#'  ## The result from the second run should contain "getting cached data" before 
#'  ## the inverse of 'mat' returned.

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

#' This function computes the inverse of the special "matrix" returned by 
#' \code{makeCacheMatrix} above. If the inverse has already been calculated 
#' (and the matrix has not changed), then the cachesolve should retrieve 
#' the inverse from the cache.
#' @param x a list object created by \code{makeCacheMatrix}, 
#' that can cache the inverse of the input matrix. 
#' @return y a matrix that is the inverse of the matrix, 'x$get()'.
#' @author Zhao Hao
#' @details
#' This function retrieve the created inverse of the matrix set up by
#' the \code{makeCacheMatrix} function. If the inverse has already been calculated
#' then the cachesolve should retrieve the inverse from the cache. Otherwise
#' solve the inverse of the matrix and store it into the cache.
#' @seealso \code{makeCacheMatrix}

cacheSolve <- function(x, ...) {
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
