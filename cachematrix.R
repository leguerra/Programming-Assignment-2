## These functions take a matrix as an input and obtains it's inverse. Since calculating the inverse of a matrix
## may be very complex, the resulting matrix is stored in cache instead of being calculated each time as long as the
## matrix is the same

## Function to store and retrieve a matrix and its inverse
# Input: Matrix to be stored 
# Output: A list of methods to storing and retriving matrixes and its inverse
makeCacheMatrix <- function(x = matrix()) {
  # i stores Null if the inverse has not been calculated yet
  i <- NULL
  # set is a inner function used to set a new matrix x
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # Returns the value of the input matrix
  get <- function() x
  # Sets the inverse of the input matrix
  setinverse <- function(inverse) i <<- inverse
  # Returns the inverse matrix or NULL
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function to effitiently calculate the inverse of a matrix
# Input: Matrix x, along with the arguments for the R solve() function
# Output: Matrix that is the inverse of the input matrix 'x'
cacheSolve <- function(x, ...) {
  # Get the store value of the inverse matrix
  i <- x$getinverse()
  # If for this matrix, the inverse has alredy been calculated (not NULL), then it is returned
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # Store the value of the input matrix
  data <- x$set()
  # Calculate the inverse of the input matrix using R's built in solve() functions, with the corresponding parameters
  i <- solve(data, ...)
  # Store the inverse matrix in cache
  x$setinverse(i)
  # Return the calculated inverse matrix
  i
}
