## Create a pair of functions that cache the inverse of a matrix

## makeCacheMatrix to cache the inverse of a matrix object

makeCacheMatrix <- function(m = matrix()) {
        y <- NULL
        set <- function(matrix) {
                m <<- matrix
                y <<- NULL
        }
        get <- function() m
        setinverse <- function(solve) y <<- solve
        getinverse <- function() y
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Compute the inverse of the special matrix returned by makeCacheMatrix.
## If inverse has been calculated, and matrix did not change,
## then "cachesolve" should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
