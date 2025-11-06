## The goal is to cache the inverse of a matrix to avoid recalculating it multiple times.
## makeCacheMatrix() → creates a special matrix object that can store (cache) its inverse.
## cacheSolve() → computes (or retrieves) the cached inverse of the matrix from makeCacheMatrix().

## This function uses closures to store data internally (matrix + its inverse)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # variable to store inverse
  
  # set: update matrix and reset cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get: retrieve matrix
  get <- function() x
  
  # setInverse: store the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # getInverse: retrieve cached inverse
  getInverse <- function() inv
  
  # return list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function checks if the inverse is already cached — if yes, it retrieves it; if not, it calculates and caches it.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # if inverse is already cached, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # otherwise compute inverse
  mat <- x$get()
  inv <- solve(mat, ...)  # compute inverse using base R
  x$setInverse(inv)       # cache it
  inv
}
