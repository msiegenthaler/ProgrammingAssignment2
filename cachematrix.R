## Function for cached operations on a matrix.

#' Create a wrapper for a matrix that caches operations. You need to use special
#' functions (cache*) instead of the normal operations (i.e. cacheSolve instead
#' of solve)
#'
#' Usage: x <- makeCacheMatrix(myMatrix)
#' To retrieve the matrix use:  x$get()
#' To change the matrix use:    x$set(myNewMatrix)
#' To get the (cached) inverse: cacheSolve(x)
makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(newX) {
        x <<- newX
        cached <<- NULL
    }
    get <- function() x
    setInverse <- function(i) cachedInverse <<- i
    getInverse <- function() cachedInverse
    list(set = set, get = get,
         setInverse = setInverse, getInverse = getInverse)
}

#' Calculates the inverse of the matrix. Uses a cache to speed up multiple calls.
#' The matrix must be wrapped by makeCacheMatrix().
#' See documentation of solve() for more information.
cacheSolve <- function(x, ...) {
    cached <- x$getInverse()
    if (is.null(cached)) {
        value <- solve(x$get(), ...)
        x$setInverse(value)
        value
    } else {
        cached
    }
}
