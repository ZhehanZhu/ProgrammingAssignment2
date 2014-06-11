## Cache the inverse of a matrix so that
## it does not have to be computed every time

## Sets and gets the value of the input matrix
## Creat extra space for the inverse of the input matrix

makeCacheMatrix <- function(x = matrix()){
	i <- NULL
	set <- function(y){
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinv <- function(inv) i <<- inv
	getinv <- function() i
	list(set=set, get=get,
		setinv = setinv,
		getinv = getinv) 
}

## Read the inverse of the input matrix if cached
## Calculate the inverse of the input matrix if not cached

cacheSolve <- function(x, ...){
	i <- x$getinv()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinv(i)
	i
}
