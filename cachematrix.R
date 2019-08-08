## Put comments here that give an overall description of what your
## functions do

## This function creates matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
                }
        get <- function() x
        setInverse <- function(inv) m <<- inv
        getInverse <- function() m
        list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)        
}

## This function computes the inverse matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated, then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
                }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
