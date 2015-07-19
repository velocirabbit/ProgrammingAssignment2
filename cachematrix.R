# Creates and returns a list with four functions:
# 1. Sets the value of the matrix
# 2. Gets the value of the matrix
# 3. Sets the value of the inverse matrix
# 4. Gets the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) i <<- inverse
    
    getinverse <- function() i
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# Solves for the inverse matrix of the matrix in the passed makeCacheMatrix
# list, then caches it. If the inverse has been found already, it returns the
# cached solution instead of resolving.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}