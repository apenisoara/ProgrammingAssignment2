## Assignment 2 for R Programming course
## Caching the Inverse of a Matrix

## Create a special matrix object that can cache a (inverse) solution

makeCacheMatrix <- function(x = matrix()) {
    saved <- NULL
    set <- function (s) {
        x <<- s
        saved <<- NULL
    }
    get <- function() x
    setsol <- function (sol) saved <<- sol
    getsol <- function () saved
    
    list(set = set, get = get, setsol = setsol, getsol = getsol)
}


## Calculates the inverse of a (cache) matrix object and tries to
## reuse a previously calculated value

cacheSolve <- function(x, ...) {
    # Try to get the solution from the cache
    ret <- x$getsol()
    if(!is.null(ret)) {
        # found cached value -- return data immediately
        message("==> Cached data !")
        return(ret)
    }
    
    # Go the hard way -- calculate the value
    data <- x$get()
    res <- solve(data, ...)
    # Save the result for a possible subsequent call
    x$setsol(res)
    # ... and return the solution
    res
}
