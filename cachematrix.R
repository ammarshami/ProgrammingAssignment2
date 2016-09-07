
## This function has one parameter of type matrix, it creates a cache of a matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the "matrix" returned by makeCacheMatrix above. If the inverse has already.
## been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse from the cache. The function
## assumes that the matrix ihas an inverse, then the new result is cached

cacheSolve <- function(x) {
  ## Return the inverse matrix of x
  inv <- x$getInverse()
  ## check if the inverse exists
  if(!is.null(inv)) {
    ## the inverse exists, return it
    return(inv)
  } else {
    ## inverse does not exist, compute it and return it
    matInv <- x$get()
    inv <- solve(matInv)
    ## cache the new inverse
    x$setInverse(inv)
    inv
  }
}

## Usage example:
## x <- matrix(1:4 , ncol=2, nrow =2)
## mat <- makeCacheMatrix(x) 
## cacheSolve(mat)

