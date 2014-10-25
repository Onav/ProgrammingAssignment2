## These two functions create a matrix vector with functions to inverse it and cache
##      the inverse so that it only needs calculating once.
## The matrix is assumed to be invertible.

## Matrix Vector
## Creates a List vector w/ functions to:
##      set matrix vector
##      Get matrix vector
##      set matrix inverse
##      get matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Calculates inverse of matrix created w/ makeCacheMatrix,
##      after checking to see if it's already been cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
