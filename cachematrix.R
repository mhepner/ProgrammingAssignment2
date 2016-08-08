## First function of Programming Assignment 2
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinverse = function(inverse) inv <<- inverse 
  getinverse = function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Second function of Programming Assignment 2
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.  If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
  inv = x$getinverse()
  
  if (!is.null(inv)){
    message("Retrieving cached data")
    return(inv)
  }
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
   x$setinv(inv)
  
  return(inv)        
}
