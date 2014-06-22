## ----------------------------------------------------------------------------
## File: cachematrix.R
##
## Put comments here that give an overall description of what your
## functions do
## ----------------------------------------------------------------------------

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL # will store the cached inverse of this matrix, or NULL to
	            # indicate that it has not yet been cached.
		
	## function to set the matrix data and initialize the cached inverse to NULL
	set <- function(y) {
		x <<- y # the “superassignment” operator <<- is required here, as in R:
		        # "any ordinary assignments done within the function are local
				# and temporary and are lost after exit from the function."
		inv <<- NULL
	}

	# function to get the matrix data
	get <- function() x
	
	## function to set the cached inverse for this matrix to the specified value
	setInverse <- function(inverse) inv <<- inverse
	
	## function to return the cached inverse for this matrix to the specified value
	getInverse <- function() inv

	## function to return the "matrix", i.e. a list that exposes the set of 4 functions
	## that define the interface for client code to make use of this enhanced matrix
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if (!is.null(inv)) {
		message("getting cached data")
	} else {
		data <- x$get()
		inv <- solve(data, ...)
		x$setInverse(inv)
	} 
	## Return a matrix that is the inverse of 'x'
	inv
}
