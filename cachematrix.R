## Caching the Inverse of a Matrix to avoid lengthy recalculations

# This function creates a special "vector", which is
# really a list containing a function to
# 1.  set the value of the vector
# 2.  get the value of the vector
# 3.  set the value of the mean
# 4.  get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The cacheSolve function takes the output from makeCacheMatrix and calculates the matrix inverse
## or returns the matrix inverse from a stored variable.

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
