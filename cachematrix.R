## This is program demostrates the caching functionality in R
## Two methods makeCacheMatrix and cacheSolve are used to "cache" and "solve the inverse" respectively

## The makeCacheMatrix is responsible for caching the inverse and implements four functions
## get and set value of the matrix
## get and set inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse, getinverse = getinverse)
}

## The cacheSolve is responsible for checking if an inverse of the matrix is cached.
## If it is, then fetch the cached value without computing; if not, then compute the inverse
## and store in cache using functions defined in makeCacheMatrix

cacheSolve <- function(x) {
        i <- x$getinverse()
        
        if(!is.null(i)) {
                message("Getting cached data...")
                return(i)
        }
        
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}