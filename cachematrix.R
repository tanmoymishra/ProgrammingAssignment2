## Below functions are used to calculate inverse of matrix
## and store the same in cache memory.

## This funtion is a combination of getter and setter for a 
## matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
	set <- function(y){
		x <- y
		inv << NULL
	}

	get <- function() x

	setinverse <- function(invs) inv <<- invs

	getinverse <- function() inv

	list(set = set, get = get, setinverse = setinverse
			getinverse <- getinverse)

}


## Below function will return the inversed matrix from cache or 
## return it by computating

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	inv <- x$getinverse()

	if (!is.null(inv)) {

		message("Returning Inversed cached matrix")
		return(inv)
	}
	
	data <- x$get()
	
	inv <- solve(data,...)
	
	x$setinverse(inv)
	
	inv
}
