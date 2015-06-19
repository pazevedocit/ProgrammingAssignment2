## MakeCacheMatrix
## This funcitions stores a InvertedMatriz on a cache in the parent scope



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverted <- function(solve) m <<- solve
  getInverted <- function() m
  list(set = set, get = get,
       setInverted = setInverted,
       getInverted = getInverted)
}


## CacheSolve - Invert the matrix only if there isn´t an inverted matrix in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverted()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverted(m)
  m
}
