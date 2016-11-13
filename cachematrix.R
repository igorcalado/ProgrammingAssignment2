## These functions take a matrix and create a special object out of it that contains both
## the original matrix and a cached value, and assigns the inverse of the matrix to the cached value,
## so it can retrieve the inverse of the matrix without having to calculate it from scratch,
## if it has already been calculated and stored.


## This function takes a matrix as an argument and creates a special object that contains the original
## matrix and a cached value where the inverse matrix will be stored by the next function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function takes the special object created by the previous functions and calculates the inverse
## of the matrix contained in that object; then, it stores the inverse of the matrix inside the cache.
## If the inverse has already been calculated once, it only retrieves the cache.
## When the inverse is being calculated for the first time for a given object, it prints the message
## "calculating" before returning the result. If the inverse matrix has already been cached for
## that object, it prints "getting cached data" and returns the cached value.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  message("calculating")
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
