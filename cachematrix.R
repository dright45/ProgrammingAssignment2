## These Functions are intended to cache matrix and then find the inverse
## after checking to see if there is cache value

## cache matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getsolve = getsolve)
}

## Solve for Inverse with Cache

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}