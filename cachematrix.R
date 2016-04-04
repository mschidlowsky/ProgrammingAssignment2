## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" which is a list containing a function
## to 1. set the value of a matrix, 2. get the value of a matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Calculates the inverse of the special "matrix" created with
## the above function.  It returns the cached inverse if it has
## already been computed.

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if (!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
