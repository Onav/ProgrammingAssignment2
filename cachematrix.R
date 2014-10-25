## Put comments here that give an overall description of what your
## functions do

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
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## Calculates inverse of matrix created w/ makeCacheMatrix,
##      after checking to see if it's already been cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- solve(data, ...)
        x$setSolve(m)
        m
}
