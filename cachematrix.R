## This pair of functions cache potentially time-consuming computations
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        b <- NULL
        set <- function(y) {
                x <<- y
                b <<- NULL
        }
        get <- function() x
        setinv <- function(solve) b <<- solve
        getinv <- function() b
        list(set = set, get = get,
              setinv = setinv,
              getinv = getinv)
}
## This function computes the inverse of x or retrieve the inverse from the cache
cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        b <- x$getinv()
        if(!is.null(b)) {
                message("getting cached data")
                return(b)
        }
        data <- x$get()
        b <- solve(data, ...)
        x$setinv(b)
        b
}