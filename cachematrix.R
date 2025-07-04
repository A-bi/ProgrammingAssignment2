## This file defines two functions to cache the inverse of a matrix.
## This is useful because matrix inversion can be computationally expensive.

## makeCacheMatrix creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Cached inverse

  # Setter function to assign a new matrix and clear the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  # Getter function to return the matrix
  get <- function() x

  # Setter for the inverse
  setinverse <- function(inverse) inv <<- inverse

  # Getter for the inverse
  getinverse <- function() inv

  # Return a list of all 4 functions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix created with makeCacheMatrix.
## If the inverse has already been calculated and the matrix hasn't changed,
## it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()

  # If inverse is already cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  # Otherwise, calculate it
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
