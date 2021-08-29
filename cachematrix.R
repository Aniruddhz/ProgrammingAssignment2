## The following pair of functions cache the inverse of a matrix

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setmat <- function(y){
                x <<- y
                inv <<- NULL
        }
        getmat <- function() x
        setinv <- function(inverse){
                inv <<- inverse
        }
        getinv <- function() inv
        list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the matrix returned by makeCacheMatrix above 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache

## It is assumed that the matrix is invertible

cacheSolve <- function(x, ...) {
        inv <- x$getinv
        if(!is.null(inv)){
                message("Getting cached data.")
                return(inv)
        }
        mat <- x$getmat
        inv <- solve(mat)
        x$setinv(inv)
        return(inv)
}
