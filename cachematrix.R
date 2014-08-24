## S. Jackson
## R Programming Assignment 2
## 

## Function takes a square matrix and creates a matrix object to cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(im) inv <<- im
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## function computes the inverse of the special "matrix" returned from makeCacheMatrix
## and return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

  im <- x$getInverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setInverse(im)
  im
}
