## The below functions use lexical scoping to provide a means for avoiding repeated calls to solve() if the matrix has not changed. 

## This function takes an invertible matrix as an argument and returns a list of 4 functions to get and set the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverse of the matrix by calling the solve() function the first time it is invoked. 
## Subsequent invocations return the pre-computed inverse.

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
