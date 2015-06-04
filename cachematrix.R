## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it repeatedly.
##
## Below are two functions that are used to create a special object
## that stores a matrix and caches its inverse.


## The first function, "makeCacheMatrix", creates a special "matrix" object
## that can cache its inverse.
##
## It actually returns a list containing functions to:
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
## 
## Note: This function assumes that the matrix supplied is always invertible.

makeCacheMatrix <- function(X = matrix()) {
        inverse <- NULL
        setMatrix <- function(Y) {
                X <<- Y
                inverse <<- NULL
        }
        getMatrix <- function() X
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        list(setMatrix = setMatrix, 
             getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## The second function, "cacheSolve", computes the inverse of the special
## "matrix" returned by "makeCacheMatrix". However, it first checks to see if
## the inverse has already been computed. If so, it gets the inverse from
## the cache and skips the computation. Otherwise, it computes the
## inverse of the matrix and sets the value of the inverse in the cache, via
## the "setInverse" function.
##
## It returns a matrix that is the inverse of "X".

cacheSolve <- function(X, ...) {
        inverse <- X$getInverse()
        if(!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
        }
        matrix <- X$getMatrix()
        inverse <- solve(matrix, ...)
        X$setInverse(inverse)
        inverse
}