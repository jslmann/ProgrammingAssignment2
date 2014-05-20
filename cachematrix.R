## "makeCacheMatrix" optionally takes a matrix for input and returns 
## a list of functions that provide "get", "set", "getinv", and "setinv"
## functions for the data held in the "x" and "inv" variables.
## git?
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(m) inv <<- m
	getinv <- function() inv
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}


## "cacheSolve" takes a list created by "makeCacheMatrix" and returns 
## the inverted matrix. The first time it is called it will calculate 
## the inverse and store it in x$inv, thereafter it will only return 
## the stored value

cacheSolve <- function(x) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinv(inv)
	inv
}
