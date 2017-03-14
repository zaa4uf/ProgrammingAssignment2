##Assignment 2 Submission

makeCacheMatrix <- function(x = numeric()) {    #this function focuses on setting, getting, 
  i <- NULL                                     # setting the inverse, and getting the inverse of our function
  set <- function(y) {                          # x must be set to numeric for our matrix calculations and pretty much
    i <<- y                                     # mirrors the example given otherwise
    x <<- NULL
  }
  get <- function() i
  setinv <- function(solve) x <<- inverse
  getinv <- function() x
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


    
    #the next functon has a similar setup with the messagefor getting cached data and has the same sequence as the given
    #example except its calculating the inverse  with the solve function rather than mean
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
