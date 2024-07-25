## the following is a pair of functions in R that cache the inverse of a matrix:

## makeCacheMatrix function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
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


## cacheSolve function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  
  inv
}

## Usage Example
## Here's how you can use the above functions to cache the inverse of a matrix:

# Create a special "matrix" object
my_matrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4), 2, 2))

# Compute the inverse and cache it
inv_matrix <- cacheSolve(my_matrix)

# Print the inverse
print(inv_matrix)

# Retrieve the cached inverse without recomputing
inv_matrix_cached <- cacheSolve(my_matrix)

# Print the cached inverse
print(inv_matrix_cached)

