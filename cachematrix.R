## These two functions enable caching the inverse of a matrix
## Usage:   j <- makeCacheMatrix(matrixToBeInverted)
##          cacheSolve(j) 
## Typically makeCacheMatrix is called only when the matrix to be inverted changes
## cacheSolve takes the returned list from makeCacheMatrix and computes the inverse

## This function returns a list, with provision for storing the inverse of
## its argument - an invertible matrix.

makeCacheMatrix <- function(x = matrix(), ...) {
	matinv <- NULL
	set <- function(y) {
		x <<- y
		matinv <<- NULL
	}
	get <- function() x
	getinv <- function() matinv
	setinv <- function(minv) matinv <<- minv
	l <- list(	set=set, get=get, 
			setinv=setinv, getinv=getinv
		)
	invisible(l)
}


## This function takes the output of the makeCacheMatrix() function
## checks if the inverse is already cached by an earlier call to this function
## if it is cached, returns the cached inverse, 
## else inverts the matrix & returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	matinv <- x$getinv() 
	if(!is.null(matinv)) {
		message("Getting cached inverse")
		return(matinv)
	}
	mat <- x$get()
	matinv <- solve(mat, ...)
	x$setinv(matinv)
	matinv
}
