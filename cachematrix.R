## Pair of functions that cache the inverse of a matrix


## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) cache <<- inverse
    getInverse <- function() cache
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    cache <- x$getInverse()
    if(!is.null(cache)) {
        message("Getting cached data")
        return(cache)
    }
    dat <- x$get()
    cache <- solve(dat)
    x$setInverse(cache)
    cache
}

