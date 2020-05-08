## Put comments here that give an overall description of what your
## functions do

## 1- create a cache of a "x" matrix object by calling first makeCacheMatrix(x) 
##		myCacheMatrix <-  makeCacheMatrix(x) 
## 2- then call the calculation of the inverse matrix on this object
##      myInverseCacheMatrix <- cacheSolve(myCacheMatrix) # the inverse is calculated at the first call
## 3... every next call to this function on this object will return the cache inverse matrix with no calculation
##		myUchangedInverseCacheMatrix <- cacheSolve(myCacheMatrix) # returns the cache object and prints a message to confirm it

## Write a short comment describing this function
	## the calculated inverse matrix will be stored in the inv variable initialized to NULL in the function closure
	## this function may be called with a first x matrix as a parameter, 
	## get() : will return simply the x matrix that has been passed 
	## setinverse() : use of the <<- parameter to store the calculated "inverse" matrix into the cache "inv" variable 
	## getinverse() : simply gets the inv cache value stored in this object environment
makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function
	## the first call of this function on a "makeCacheMatrix" will call getinverse() and find the cache inv variable valuated to NULL
	## so it gets the initial "x" matrix by calling the get() function, calculates the inverse matrix and stores it 
	## in the cache value by calling setinverse(). it returns this inv calculated matrix at the first call.
	
	## the next calls to this function will find a cache value when calling getinverse(). 
	## Then it simply returns this value if not NULL and prints a message to confirm that the cache is used.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv

}
