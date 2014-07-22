## Matrix inversion caching functions
## Because matrix inversion can be a costly process, it is useful to be able
## to create a matrix which, following one inversion, caches its inverse
## so that subsequent calls (assuming the matrix has not changed) retrieve the
## cached inverse value rather than re-computing it.

## TESTING OF THE BELOW FUNCTIONS
## Can easily be tested by issuing the following commands at the R prompt (obviously
## need to remove the comments first)
## mat = matrix(c(1,2,3,4), nrow=2, ncol=2)
## mat2 = matrix(c(2,3,4,5), nrow=2, ncol=2)
## cacheMat = makeCacheMatrix(mat)
## inv = cacheSolve(cacheMat) (check that inv is the inverse of mat)
## inv = cacheSolve(cacheMat) (see output to show the cache was used)
## cacheInv = makeCacheMatrix(inv)
## newMat = cacheSolve(cacheInv) (newMat should be the same as mat)
## newMat = cacheSolve(cacheInv) (see output to show the cache was used)
## cacheMat$set(mat2)
## inv2 = cacheSolve(cacheMat) (should recalculate and not use cache)
## inv2 = cacheSolve(cacheMat) (should use cache, and there should be output showing cache was used)
## For completeness, check that solve did what was expected:-
## mat %*% inv returns the unit matrix

# Wrap a matrix in a wrapper which caches the inverse so it only needs to be 
# calculated once
makeCacheMatrix <- function(x = matrix()) {
    # initially we haven't calculated the inverse so we make it NULL
    inverse <- NULL

    # set updates the value of the contained matrix, so we need to 
    # throw away any inverse value we've already calculated
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    # get simply returns the matrix we are wrapping
    get <- function() x

    # set the cached inverse value
    setinverse <- function(inv) inverse <<- inv

    # retrieve the cached inverse value
    getinverse <- function() inverse 

    # Return "object" containing data and methods
    list(set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
        )
}

# Invert the matrix, but use the cached inverse if it has already
# been calculated for the currently wrapped matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # First check if we've already calculated the inverse of x
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("retrieved inverse from cache")
        return (inv)
    }

    # We hadn't previously calculated the inverse of the current
    # matrix wrapped by x, so we must do so now
    mat <- x$get()
    inv <- solve(mat)

    # Now we've calculated the inverse, cache it for further use
    x$setinverse(inv)
    inv
}
