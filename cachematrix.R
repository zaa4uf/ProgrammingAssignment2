
makeCacheMatrix <- function(x = numeric()) {
  i <- NULL
  set <- function(y) {
    i <<- y
    x <<- NULL
  }
  get <- function() i
  setinv <- function(solve) x <<- inverse
  getinv <- function() x
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
