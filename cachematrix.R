## These functions will cache the results of calculating the inverse
## of a matrix. The "super assignment" operator is used to store the
## cached result in the enclosing environment so its value persists
## after the function exits.

makeCacheMatrix <- function(x = matrix()) {
  # Creates a wrapped matrix object that can cache its inverse.
  #
  # Args:
  #   x: a matrix [optional] if no matrix is supplied an empty matrix is created
  #
  # Returns:
  #   a list object with functions to get and set the wrapped matrix and its inverse
  #
  # Setting a matrix will clear the cached inverse until it is next calculated.
  #
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  # This function computes the inverse of the wrapped "matrix" returned by
  # 'makeCacheMatrix'. If the inverse has already been calculated (and the
  # matrix has not changed), then this function will retrieve the inverse
  # from the cache.
  #
  # Args:
  #   x: the wrapped matrix returned by 'makeCacheMatrix'
  #   ...: additional arguments to 'solve'
  #
  # Returns:
  #   Return a matrix that is the inverse of 'x'
  #
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
