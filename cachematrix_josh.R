## These two functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
# First set to NULL
# Changes with a set value
  inv <- NULL

# Sets the matrix, but not its inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Gets the matrix, but not its inverse
  get <- function() x
  
  # Sets the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Gets the inverse
  getinverse <- function() inv
  
  # Encapsulates the inverse into a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)	
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # Gets the current state of the inverse and assess whether
  # or not it has been computed
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    # Return the computed inverse		
    message("Getting cached matrix")
    return(inv)
  }
  
  # If the current state of the inverse has not been computed,
  # get the matrix 
  data <- x$get()
  
  # Finds the inverse
  inv <- solve(data, ...)
  
  # Caches the result 
  x$setinverse(inv)
  
  # Returns a new result
  inv
}
