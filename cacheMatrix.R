# Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the cache
  inv <- NULL
  
  # Define the set function to set the matrix and reset the cache
  set <- function(matrix) {
    x <<- matrix
    inv <<- NULL
  }
  
  # Define the get function to return the matrix
  get <- function() x
  
  # Define the setinv function to set the cached inverse
  setinv <- function(inverse) inv <<- inverse
  
  # Define the getinv function to return the cached inverse
  getinv <- function() inv
  
  # Return a list with the above functions as its elements
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# Compute the inverse of the matrix stored in a "matrix" object
cacheSolve <- function(x, ...) {
  # Get the cached inverse if available
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If the inverse is not cached, compute it
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setinv(inv)
  
  # Return the inverse
  inv
}


# create matrix
m <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)

# create cache matrix object
cm <- makeCacheMatrix(m)

# compute inverse and cache it
cacheSolve(cm)

# retrieve cached inverse
cacheSolve(cm)

# modify matrix
m[1, 1] <- 5

# try to retrieve cached inverse (should recalculate)
cacheSolve(cm)



