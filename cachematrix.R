## This functions are used for calculating and caching the inverse of a matrix.

## Creates a special matrix that can store the inverse of the input matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Return the cached inverse if the matrix is the same or calculate the new inverse.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        return(m)
    }
    m <- solve(x$get())
    x$setsolve(m)
    m
}