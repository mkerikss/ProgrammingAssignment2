## This file contains two function used to solve the inverse of a matrix, using cached memory
## if available. This ensures that the computationally heavy calculations involved in 
## calculating the inverse of a matrix are not performed if the inverse has already been 
## calculated and the matrix hasn't changed.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse,
## which is really a list containing functions to 
## - set the value of the matrix
## - get the value of the matrix
## - calculate the inverse matrix
## - get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
## not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse   
    
}
