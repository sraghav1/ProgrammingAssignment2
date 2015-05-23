## Functions to build a mechanism to cache matrix inverse

## This function caches the matrix and its inverse
## It exposes getters and setters to access the cache.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


## Optimized solver to compute inverse of a matrix
## Operates on a matrix object (list) returned by makeCacheMatrix function
## Tries to read the inverse from cache. if not found, computes, updates cache 
## and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                return(inv)
        }
        else {
                inv <- solve(x$get())
                # Assuming the matrix is always invertible
                x$setinverse(inv)
                return(inv)
        }
}
