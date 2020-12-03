## Put comments here that give an overall description of what your
## functions do

## Function 1: Creates a special matrix "object" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv.x <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function(x)
	inverted.matrix <- function(inverse) inv.x <<- inverse
	get.inverse <- function() inv.x
	list(set = set, get = get, inverted.matrix = inverted.matrix, get.inverse = get.inverse)
}


## Function 2: Computes the inverse of the "special" matrix returned by Function 1
## If the inverse is already calculated, the matrix is returned.  Otherwise,
## the function computes it, caches it, and returns it

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
	if(!is.null(m)){
		print('fetching cached inverse matrix')
		return(m)
	} else {
		m <- solve(x$get)
		x$setinverse(m)
		return(m)
	}
}
