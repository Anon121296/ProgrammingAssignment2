## A set of functions that allow one to create a special matrix 
## that automatically caches its inverse, which can then be 
## retrived 


## makeCacheMatrix creates a matrix from a provided dataset and 
## caches the inverse of the matrix, which can be retrived by 
## cacheSolve. In sum, it is a list that: sets the value of the 
## matrix, gets that value, sets the value of the inverse of the 
## matrix, and gets that value (stored in cache)

makeCacheMatrix <- function(x = matrix()) { 
	i <- NULL
	set <- function(y) { 
		x <<- y
		i <<- NULL
	} 
	get <- function() x
	set_inverse <- function(inverse) i <<- inverse
	get_inverse <- function() i 
	list(set = set, get = get, 
		set_inverse = set_inverse,
		get_inverse = get_inverse)
} 


## cacheSolve retrives the cached inverse of the matrix (x) stored 
## in makeCacheMatrix. If not previously computed, computes 
## inverse of the matrix itself.

cacheSolve <- function(x, ...) { 
	i <- x$get_inverse()
	if(!is.null(i)) { 
		message("getting cached data")
		return(i) 
	} 
	data <- x$get()
	i <- solve(data, ...)
	x$set_inverse(i) 
	i
} 
