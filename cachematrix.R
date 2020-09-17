##The following functions creates a special matrix and defines a few functions to be able to cache
##the inverse of a matrix entered into the function and then check if it has been cached and return 
##the inverse matrix accordingly.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 i <- NULL
 set <- function(y) {
       x <<- y
       i <<- NULL
}
 get <- function() x
 setinv <- function(inverse) i <<- inverse
 getinv <- function() i
 list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
 i <- x$getinv()
 if(!is.null(i)) {
    message("getting cached data")
    return(i)
}
 data <- x$get()
 i <- solve(data, ...)
 x$setinv(i)
 i 
}
