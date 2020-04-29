## The function makeCacheMatrix creates a matrix object that can cache its inverse ("inv").
## Then, cacheSolve checks if "inv" has already been defined. If yes, it returns it.
## Otherwise, it gets the matrix and calculates its inverse

## makeCacheMatrix sets the value of the "special matrix" (with set) and caches its inverse ("inv", with "setinv").

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve checks if "inv" has already been defined in x. If yes, it returns it.
## Otherwise, it gets the matrix in x, calculates its inverse and returns it.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}