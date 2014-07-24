## Purpose: These set of functions illustrate the usage of lexical scoping in R
## 		to cache the inverse of a matrix. The general idea is to create a
##		matrix-like class with getter and setter functions. A wrapper function
##		uses these getters and setters to return the cached result, if it
##		exists. It sets the inverse if it doesnt exist.
## Author: Vijesh
## email: mv.vijesh@gmail.com

## This function creates an "object" that wraps a matrix along with
## a set of getter and setter functions in a list.
makeCacheMatrix <- function(x = matrix()) { ## an empty matrix is set as default
	inverse <- NULL

	## setter and getter functions for the matrix
	setMatrix <- function(y) {
		x <<- y
		inverse <<- NULL
	}

	getMatrix <- function() x

	## setter and getter functions for the inverse of the matrix
	setInverse <- function(inv) inverse <<- inv
	
	getInverse <- function() inverse

	## return a list with 
	list(setMatrix = setMatrix,
		getMatrix = getMatrix,
		setInverse = setInverse,
		getInverse = getInverse)
}

## This function is a wrapper around the getter function for the "matrix"
## list. If the cached inverse is available, it returns the cached value.
## If the cached inverse isnt available, it generates the inverse and 
## caches it.
cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if(!is.null(inv)) {
		## the inverse was already computed. return the stored value.
		message("Getting cached matrix inverse..")
		return(inv)
	}

	## the matrix was newly set and the inverse isnt computed yet. compute, store it and return it.
	mtrx <- x$getMatrix()
	inv <- solve(mtrx, ...)
	x$setInverse(inv)
	inv
}