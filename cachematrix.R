## functions compute a matrix inversion, cache the result, and reuse that cache.  

## makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to
# set the matrix
# get the matrix
# set the inverse of the matrix
# get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function uses R base function solve to calculate the inverse of the special
## "matrix" from makeCacheMatrix.  However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache and
## skips the computation. Otherwise, it calculates the inverse of the matrix and
## sets inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matriks <- x$get()
  inv <- solve(matriks, ...)
  x$setinverse(inv)
  inv
}
