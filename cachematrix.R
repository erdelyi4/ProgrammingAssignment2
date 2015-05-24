## Two functions are defined in this file 
## As matrix inversion requires a large amount of CPU time it is assumed 
## that by caching the computed inverse the code can run faster
## These functions just do that

## makeCacheMatrix creates a list of functions that can be applied to the argument
## Arg: an invertible matrix
## Return value: the functions that can be used to manipulate the argument

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## set = creates a new matrix and deletes the cached inverse
    set <- function(y){ 
        x <<- y
        m <<- NULL
    }
    ## get = returns the matrix
    get <- function() x
    ## setinverse = caches the calculated inverse matrix
    setinverse <- function(solve) m <<- solve
    ## getinvers = returns the cached inverse matrix
    getinverse <- function() m
    ## the return value
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve calculates the inverse of the matrix or returns it from the cache
## if it is already calculated
## Arg: the special matrix that is created with makeCacheMatrix function
## Return value: the inverse of the matrix that was the argument of 
## the makeCacheMatrix function

cacheSolve <- function(x, ...) {
    ## probe the cache    
    m <- x$getinverse()
    ## if the inverse is already calculated no new calculation is necessary just
    ## return the value
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    ## otherwise assign the matrix argument from the special matrix created with
    ## makeCacheMatrix to data variable
    data <- x$get()
    ## calculate the inverse of that matrix
    m <- solve(data, ...)
    ## put the inverse into cache
    x$setinverse(m)
    ## return with the inverted matrix
    m
}
