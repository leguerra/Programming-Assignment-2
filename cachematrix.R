## These functions take a matrix as an input and obtains it's inverse. Since calculating the inverse of a matrix
## may be very complex, the resulting matrix is stored in cache instead of being calculated each time as long as the
## matrix is the same

## Function to store and retrieve the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function to calculate the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  data <- x$get()
  if(!is.null(i) && (x == data)) {
    message("getting cached data")
    return(i)
  }
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
