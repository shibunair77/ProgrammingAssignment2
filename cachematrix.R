##Shibu Kumar Nair

## Create a Matrix object to cache inverse
## With Functions
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y){
			x <<- y
			inv <<- NULL
		}
		get <- function() x
		setInverse <- function(data) inv <<- data
		getInverse <- function() inv
		list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse when not cached.

cacheSolve <- function(x, ...) {
		## Return a matrix that is the inverse of 'x'
		inv <- x$getInverse()
		if(!is.null(inv)){
			message("Returning from cache")
			return(inv)
		 }else{
			data <- x$get()
			inv <- solve(data)
			message("Caching data")
			x$setInverse(inv)
			inv   
		}
}
