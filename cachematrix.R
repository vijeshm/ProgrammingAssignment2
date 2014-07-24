## Purpose: These set of functions illustrate the usage of lexical scoping in R
## 		to cache the inverse of a matrix. The general idea is to create a
##		matrix-like class with getter and setter functions. A wrapper function
##		uses these getters and setters to return the cached result, if it
##		exists. It sets the inverse if it doesnt exist.
## Author: Vijesh
## email: mv.vijesh@gmail.com

## This function creates an "object" that wraps a matrix along with
## a set of getter and setter functions in a list.
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}

	get <- function() x

	setInverse <- function(inv) inverse <<- inv
	
	getInverse <- function() inverse

	list(set = set,
		get = get,
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
		message("Getting cached matrix inverse..")
		return(inv)
	}

	mtrx <- x$get()
	inv <- solve(mtrx, ...)
	x$setInverse(inv)
	inv
}