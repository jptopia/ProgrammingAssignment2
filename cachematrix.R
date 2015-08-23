## R Programming - Programming Assignment 2
## August 23, 2015

## Create a special matrix object that will cache the inverse matrix
## calculation so the procedure only needs to be executed once

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(m) {
    x <<- m
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return the stored inverse matrix if available.
## If the inverse matrix has not been calculated,
## solve the inverse, store and return the result

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cahced inverse matrix.")
    return(inv)
  }
  inv <- solve(x$get(), ...)
  x$setinv(inv)
  inv
}
