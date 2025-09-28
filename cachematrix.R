# Function to create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  # Set the matrix
  set <- function(y) {
    x <<- y  # Assign the matrix
    inv <<- NULL  # Reset the cached inverse
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the inverse
  setInverse <- function(inverse) inv <<- inverse 
  
  # Get the inverse
  getInverse <- function() inv
  
  # Return a list of functions to manipulate the matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Function to compute the inverse of the special matrix if not cached
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Try to get the cached inverse
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)  # Return cached inverse
  }
  
  # If the inverse is not cached, get the matrix
  data <- x$get()
  
  # Compute the inverse using the solve function
  inv <- solve(data, ...)
  
  # Cache the computed inverse
  x$setInverse(inv)
  
  inv  # Return the computed inverse
}