## Create a special matrix object that stores a matrix and its inverse.
## A cache function calculates the inverse and stores in within the matrix object.


## Takes a matrix and returns an list of getter and setter functions to 
## store the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Takes a matrix list object and calculates its inverse
## Inverse is stored in the matrix list object
## If inverse is already calculated then it return the value without recalculating.
cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data)
	x$setinverse(i)
	i
}
