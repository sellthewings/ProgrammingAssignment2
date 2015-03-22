
## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(cache1 = matrix()) {
    m <- NULL
    getfunction <- function() cache1
    setfunction <- function(y) {
        cache1 <<- y
        m <<- NULL
    }
    getmatrixfunction <- function() m
    setmatrixfunction <- function(solve) m <<- solve
    list(set = setfunction, get = getfunction,
         setmatrix = setmatrixfunction,
         getmatrix = getmatrixfunction)
}


## This function computes the inverse of the matrix object returned 
##by makeChacheMatrix. If the inverse has already been caclulated
## and the martrix is unchanged, cacheSolve retrieves the inverse
##from the cache

cacheSolve <- function(cache, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- cache$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- cache$get()
    m <- solve(data, ...)
    cache$setmatrix(m)
    m
}

