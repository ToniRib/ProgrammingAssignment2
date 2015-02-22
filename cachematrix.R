## These functions compute the inverse square of an invertible matrix, x
## and store it to a cache for later use.
## If the inverse of the matrix (x) is already stored in a cache,
## cacheSolve will return the value of the inverted matrix already in the cache.

## Original code written by Toni M Rib on February 21, 2015.
## ---------------------------------------------------------

## makeCacheMatrix creates a "matrix" object (which is actually a list) that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <-function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}


## cacheSolve computes the inverse of the "matrix" (which is actually a list) returned
## by makeCacheMatrix. If the inverse has already been calculated (and the matrix has
## not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
