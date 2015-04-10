## Cache the Inverse of a Matrix

## Create a matrix capable of caching its inverse

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cache <<- inverse
  getInverse <- function() cache
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  
}


## Same as `solve()`, but uses a cacheable matrix (use with `makeCacheMatrix`)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cache <- x$getInverse()
  if (!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  data <- x$get()
  cache <- solve(data, ...)
  x$setInverse(cache)
  cache
}
