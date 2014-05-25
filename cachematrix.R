## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ret <- x$getsol()
    if(!is.null(ret)) {
        message("==> Cached data !")
        return(ret)
    }
    
    data <- x$get()
    res <- solve(data, ...)
    x$setsol(res)
    res
}
