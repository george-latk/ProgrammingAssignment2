## ----------------------------------------------------------------------------
## File: cachematrix.R
##
## Contains 2 functions that provide an enhanced matrix object which caches its
## inverse.
##
## A typical usage scenario:
##     > a <- matrix(1:4, nrow = 2, ncol = 2)
##     > m <- makeCacheMatrix(a) # create enhanced matrix
##     > m$get()                 # get the matrix data underlying m
##          [,1] [,2]
##     [1,]    1    3
##     [2,]    2    4
##     > cacheSolve(m)           # calculate the inverse of m
##          [,1] [,2]
##     [1,]   -2  1.5
##     [2,]    1 -0.5
##     > cacheSolve(m)           # do again, note use of cache message
##     getting cached data
##          [,1] [,2]
##     [1,]   -2  1.5
##     [2,]    1 -0.5
##
##     > x <- matrix(c(1,1,2,0,2,0,1,1,1), nrow = 3, ncol = 3)
##     > m$set(x)                # change the underlying data
##     > cacheSolve(m)           # solve again, note cache has been reset
##          [,1] [,2] [,3]
##     [1,] -1.0  0.0    1
##     [2,] -0.5  0.5    0
##     [3,]  2.0  0.0   -1
##
## TODO: This is the design specified by the assignment, however, it appears
##       that a better design would be one that eliminates the method cacheSolve
##       and instead incorporates the logic of its body as the getInverse function 
##       defined in makeCacheMatrix.
##       This allows us to eliminate the function setInverse, which should not be
##       used by the client of this code (which would be highly problematic),
##       and to hide the internals exposed by the distinction between getInverse
##       and cacheSolve. 
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
	## This method needed for internal use by the cacheSolve function.
	setInverse <- function(inverse) inv <<- inverse
	
	## function that returns the cached inverse for this matrix, or null to indicate
	## that the inverse has not yet been calculated.
	## This method needed for internal use by the cacheSolve function.
	getInverse <- function() inv

	## Create the return value of this function, an enhanced "matrix",
	## i.e. a list that exposes the set of 4 functions that define the
	## interface for client code to make use of this enhanced matrix
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.
## This method uses R's solve, and is a replacement for that method when
## using an enhanced matrix.
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
