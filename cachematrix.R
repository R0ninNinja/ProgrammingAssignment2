## Title  	  : Caching the Inverse of a Matrix
## Done by	  : R0ninNinja
## Date		  : August 21, 2016

## Description: Matrix inversion is usually a costly computation and there
##		may be some benefit to caching the inverse of a matrix rather
##		than compute it repeatedly. Below are a pair of functions that
##		cache the inverse of a matrix.


## The function "makeCacheMatrix" creates a special "matrix" object that can
## cache its inverse, which is a list containing a function to:
## 	- set the value of the matrix
## 	- get the value of the matrix
## 	- set the value of the inverse matrix
## 	- get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	invertible <- NULL
	set <- function(y) {
		x <<- y
		invertible <<- NULL
	}
    get <- function() x
    setInverse <- function(inverse) invertible <<- inverse
    getInverse <- function() invertible
    list(set = set,
    	 get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The function "cacheSolve" computes the inverse of the special "matrix"
## returned by "makeCacheMatrix" above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	invertible <- x$getInverse()
	if (!is.null(invertible)) {
		message("getting cached data")
		return(invertible)
	}
	matrixData <- x$get()
	invertible <- solve(matrixData, ...)
	x$setInverse(invertible)
	invertible
}

