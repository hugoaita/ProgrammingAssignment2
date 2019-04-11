## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Make a matrix whose inverse can be cached
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set=set, get=get,
       getinverse = getinverse,
       setinverse = setinverse)
}


## Write a short comment describing this function
# Calculates the inverse of the special "matrix"
# calculated with the above function.
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
