testCacheMatrix <- function() {
    testCacheSolveShouldProduceSameValueAsSolve()
    testCacheSolveShouldAlwaysProduceTheSameValue()
}

assertEquals <- function(a, b) {
    if (xor(is.null(a), is.null(b)) || a != b)
        stop(paste(toString(a), "is not equal to", toString(b)))
}

testCacheSolveShouldProduceSameValueAsSolve <- function() {
    m <- matrix(c(1,2,3,0,1,4,5,6,0), 3, byrow = TRUE)
    cached <- makeCacheMatrix(m)
    expected <- solve(m)
    is <- cacheSolve(cached)
    assertEquals(is, expected)
}

testCacheSolveShouldAlwaysProduceTheSameValue <- function() {
    m <- matrix(c(1,2,3,0,1,4,5,6,0), 3, byrow = TRUE)
    cached <- makeCacheMatrix(m)
    fst <- cacheSolve(cached)
    assertEquals(fst, cacheSolve(cached))
    assertEquals(fst, cacheSolve(cached))
    assertEquals(fst, cacheSolve(cached))
}

