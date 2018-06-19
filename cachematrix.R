## These functions creates and manages a special matrix object through defined functions.

## This function creates an empty matrix and its empty inverse
## It also defines four functions to manage the matrix and its inverse:
## - get (gets matrix)
## - set (sets matrix)
## - getinverse (gets matrix inverse)
## - setinverse (sets matrix inverse)

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function gets the cached inverse of its argument.
## If there is no cached inverse, the inverse of the argument is calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m

}
