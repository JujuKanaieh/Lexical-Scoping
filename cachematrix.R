## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  # Setter function to set the matrix
  set <- function(y) {
    x <<- y      # Assign the new matrix to x in the parent environment
    inv <<- NULL # Reset the cached inverse to NULL
  }
  
  # Getter function to get the matrix
  get <- function() x
  
  # Setter function to set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Getter function to get the inverse
  getInverse <- function() inv
  
  # Return a list of functions
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse() # Retrieve the cached inverse if it exists
  
  if (!is.null(inv)) {
    message("getting cached data") # Notify if using cached data
    return(inv)
  }
  
  mat <- x$get()        # Get the matrix
  inv <- solve(mat, ...) # Calculate the inverse
  x$setInverse(inv)     # Cache the inverse
  
  inv
}
