## makeCacheMatrix function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse_X <- NULL
  set <- function(y) {
    x <<- y
    inverse_X <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inverse_X <<- inverse
  getInverse <- function() inverse_X
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_X <- x$getInverse()
  if (!is.null(inverse_X)) {
    message("getting cached data")
    return(inverse_X)
  }
  mat <- x$get()
  inverse_X <- solve(mat, ...)
  x$setInverse(inverse_X)
  inverse_X
}
