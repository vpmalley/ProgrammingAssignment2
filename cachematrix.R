
## These two functions together allow to cache the inverse of a matrix and compute it when not cached.
## The first function 'makeCacheMatrix' creates a data structure storing a matrix and its inverse.
## The second function uses the previous structure to either return the cached inverse or compute and store it.


## Builds a structure containing a matrix and its cached inverse, accessible through functions.
##
## The functions are set, get, setinverse and getinverse, made to set/get the initial matrix and inverse.
## For instance, 'x <- makeCacheMatrix' and 'x$set(y)' will create the structure 
## and set the 'y' matrix as the initial matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        # defining functions to set/get the initial matrix ...
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        # ... as well as its inverse
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        # constructing a list of functions to apply to the stored matrix and its inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Determines the inverse of matrix in x, either from the cached inverse or by computing it

cacheSolve <- function(x, ...) {
        ## First we check the content of the cache for the inverse
        inv <- x$getinverse()
        if(!is.null(inv)) {
                ## if the inverse is cached
                message("getting cached data")
                return(inv)
        }
        ## if the cache is empty, we compute the inverse of the matrix in x
        data <- x$get()
        inv <- solve(data, ...)
        ## and we update the cached inverse
        x$setinverse(inv)
        inv
}
