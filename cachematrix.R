## Provides a special Matrix that can cache it's inverse

## Creates a matrix that is capable of caching it's inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y)
    inverse <<- NULL
    x <<- y
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of a cache matrix.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
