## These functions are for programming assingment number 2 
## in the R Programming class. These support Caching the 
## Inverse of a Matrix to avoid unneccesary costly computation.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     i <- matrix(NA,ncol=ncol(x),nrow=nrow(x))
     set <- function(y) {
          x <<- y
          i <<- matrix(NA,ncol=ncol(x),nrow=nrow(x))
     }
     get <- function() x
     setinverse <- function(inv) i <<- inv
     getinverse <- function() i
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     ## Check if the inverse has been cached
     i <- x$getinverse()
     if(!all(is.na(i))) {
          message("getting chached data")
          return(i)
     }
     ## If not cached, then create the inverse, cache it, and return it
     data <- x$get()
     i <- solve(data,...)
     x$setinverse(i)
     i
}
