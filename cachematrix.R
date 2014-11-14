## Exercise to demonstrate lexical scoping and
## use of super assignment operator (<<-)
## x<<-y means x in the parent environment
## attains the value of y in the local environment


## makeCacheMatrix - 
## Can be used to set the value of vector
## Get the matrix
## Set value of inverse
## Get value of inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # the inverse is reset to NULL
	        # everytime makeCacheMatrix is called
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() {x}
## change inv from null to actual inverse    
    setinverse <- function(inverse)
    {    inv <<- inverse }
    getinverse <- function() {inv}
    list(get = get, setinverse = setinverse, getinverse = getinverse)
}

## Cachesolve receives an object of makeCacheMatrix
## as argument
## For a newly created vector(object), 1 . this function finds
## and returns inverse, 2. call setinverse(), to change 
## inv from null to actual inverse;
## thus becoming cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
## If cached  
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
## If a new vector is created or passed using set,
## is.null(inv) TRUE
## the following gets executed
	data <- x$get()
	inv <- solve(data,...)
	x$setinverse(inv)
	inv
}
