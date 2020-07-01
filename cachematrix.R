## Put comments here that give an overall description of what your
## functions do

# Generates matrix cache for avoid recalculating matrix inverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
      set <- function(y) {
		x <<- y
		m <<- NULL
      }
      get <- function() x
      setinverse <- function(inv) m <<- inv
      getinverse <- function() m
      list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


# return an inverse matrix in cache, if there is no matrix in cache it
# calculates the inverse of x and cache it

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
	if(!is.null(m)){
		message('Returning cached inverse matrix')
		return(m)
	}
	mat <- x$get()
	inv <- solve(mat)
	x$setinverse(inv)
	inv
}
