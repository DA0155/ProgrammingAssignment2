## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list of functions that can
## accept an input matrix and cache it's inverse.

## cacheSolve returns the inverse matrix from the 
## cached value if it exists or by calculating 
## and caching the inverse.


## Write a short comment describing this function
## 
makeCacheMatrix <- function(x = matrix()) {

	## Creates a list of functions that
	## can cache the inverse of a matrix.
	
	# Clear the cache m and store the
	# value of the input matrix in x
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
	
	# Get operation returns the value of the input matrix
    get <- function() x
	
	# Set operation accepts the inverse of the matrix
    setInverse <- function(inverse) m <<- inverse
	
	# Get operation returns the inverse of the matrix m
    getInverse <- function() m
	
	# Create list containing all of the above 
	# functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
	## If a cached value exists, the cached value will be returned.
	## Otherwise the inverse is calculated, cached, and returned.
   
	# Get the matrix inverse from 
	# the cached value and assign to m
	m <- x$getInverse()
   
	# Check if m is null.  
	# If not null, return the value of m.
    if ( ! is.null(m)) {
        print("getting cached data")
        return(m)
    }
	
	# If m is null (not cached),
	# get the original matrix and calculate it's inverse.
	# Assign the inverse to m
	m <- solve(x$get())
	
	# Call the set function to store the matrix inverse 
	# in the cache and return m
    x$setInverse(m)
    m
}
