## Function 1: Creates speical "matrix" object that is able to 
## cache its inverse

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


## Function 2: Computes inverse of matrix constructed by Function 1.  If 
## the inverse of the matrix has already been calculated, then Function 2 
## retrieves the inverse from the cache.

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
