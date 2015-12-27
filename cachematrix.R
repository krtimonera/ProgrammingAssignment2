## These functions calculate the inverse of a matrix and cache the inverse so that 
## it can be retrieved without repeating the potentially expensive calculation if it
## has been calculated previously.

## The 'makeCacheMatrix' function creates a special matrix that can cache its inverse.

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
             setinverse = setinverse,
             getinverse = getinverse)
}


## The 'cacheSolve' function takes a matrix 'x' and returns its inverse.
## If the inverse of the given matrix has previously been calculated, it will
## retrieve the value from the cache. If it has not, it will calculate the
## inverse and save it to the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}