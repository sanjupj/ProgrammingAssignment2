## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below 2 functions are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        setmatrix <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        getmatrix  <- function() x
        
        setinverse <- function(inv) inverse <<- inv
        
        getinverse <- function()inverse
        
        list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)
}

## This function returns the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated, 
## then it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        
        mymatrix <- x$getmatrix()
        inverse  <- solve(mymatrix)
        x$setinverse(inverse)
        inverse
}
