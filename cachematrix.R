## A pair of functions are written that caches the inverse of a matrix.

## This function creates a special vector and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL 
      set <- function(y) {
        x <<- y
        m <<- NULL 
      }
      get <- function() x 
      setinverse <- function(solve) m <<- solve 
      getinverse <- function() m 
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function will calculate the inverse of the special matrix or if already
## calculated it returns the calculated inverse of the matrix 

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$get()
      m <- solve(data,...)
      x$setinverse(m)
      m
}
