## makeCacheMatrix will create a matrix that caches its inverse after the first call to 
##   make computations faster.
## cacheSolve will inverse the matrix and cache the inverse so next calls to it will be
##   faster.

## Create a matrix that caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse matrix. The first time it is run, put it in a cache.
## so that further calls to it will return the cached value instead of
## recomputing it.

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