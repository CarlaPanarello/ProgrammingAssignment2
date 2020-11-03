## Caching the inverse of a matrix

## The function makeCacheMatrix creates a special "matrix" object
## that can cache its inverse.
## When the matrix changes, the cache is cleared

makeCacheMatrix <- function(x = matrix()) {
    xInv <- NULL
    set <- function(y) {
        x <<- y
        xInv <<- NULL
    }
    get <- function() x
    setInv <- function(i) xInv <<- i 
    getInv <- function() xInv
    list (set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated, 
## then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    xInv <- x$getInv()
    if (!is.null(xInv)) { 
        message("Getting cached data")
        return(xInv)
    }
    xInv <- solve(x$get(), ...)
    x$setInv(xInv)
    xInv
}
