## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    temp <- NULL
    set <- function(y) {
        x <<- y
        temp <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) temp <<- solve
    getinverse <- function() temp
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    temp <- x$getinverse()
    if(!is.null(temp)) {
        message("getting cached data")
        return(temp)
    }
    data <- x$get()
    temp <- solve(data, ...)
    x$setinverse(temp)
    temp
}
