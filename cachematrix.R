## Calculating the inverse of a matrix is computationally expensive. This code allows the 
## matrix inverse to be stored in cache after it is computed for the first time

## The makeCacheMatrix function takes a matrix as input and returns a cacheable object, that is,
## a list of functions to store("set") and retrieve ("get") the matrix itself, and to
## store ("setinverse") and retrieve ("getinverse") its inverse (initialized to NULL).
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


## The cacheSolve function takes a makeCacheMatrix type object and returns the inverse of the 
## matrix it contains. If the inverse has been calculated before, it is retrieved using the
## makeCacheMatrix$getinverse method; if not, it is computed using the solve() function and
## stored for later use by the makeCacheMatrix$setinverse method.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
