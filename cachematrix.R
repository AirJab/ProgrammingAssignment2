
# This function, makeCacheMatrix, creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    inv_1 <- NULL
    set <- function(y) {
        x <<- y
        inv_1<<- NULL
    }
    get <- function() x
    setinverse <- function(inverse_1) inv_1<<- inverse_1
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    cacheSolve <- function(x, ...) {
        inv_1 <- x$getinverse()
        if(!is.null(inv_1)) {
            message("getting cached data")
            return(inv_1)
        }
        data <- x$get()
        inv_1 <- inverse_1(data, ...)
        x$setinverse(inv_1)
        inv_1
    }
    
}
