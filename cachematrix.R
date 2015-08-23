## R Programming - Programming Assignment 2
## August 23, 2015

## Create a special matrix object that will cache the inverse matrix
## calculation so the procedure only needs to be executed once.
## The function returns a list with getter and setter functions
## for both the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  
  ## Matrix setter function
  set <- function(m) {
    x <<- m
    inv <<- NULL
  }
  
  ## Matrix getter function
  get <- function() x
  
  ## Inverse setter function
  setinv <- function(inverse) inv <<- inverse
  
  ## Inverse getter function
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return the stored inverse matrix if available.
## If the inverse matrix has not been calculated,
## solve the inverse, store and return the result

cacheSolve <- function(x, ...) {
  ## Get the inverse matrix for cache matrix object x
  inv <- x$getinv()
  
  ## Return inverse if it has already been cached
  if(!is.null(inv)) {
    message("Getting cahced inverse matrix.")
    return(inv)
  }
  
  ## If inverse is NULL, solve the inverse, store the
  ## matrix using the getter function, and return
  ## the result
  inv <- solve(x$get(), ...)
  x$setinv(inv)
  inv
}
