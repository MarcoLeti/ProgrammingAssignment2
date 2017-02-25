## the code below calculates the inverse of a matrix and stores
## its value on the cache. Until the variables doesn't change
## the value of the inverse of the matrix is retrieved from the cache.
## If not it is recalculated.

## the function solve() calculates the inverse of a matrix
## if we put just the first argument "a", see ?solve for details

## the function "makeCacheMatrix" is a list of 4 functions:
## a the begin the variable x and i are inizialized as "empty/null"
## the first function sets the value of the matrix
## the second gets the value of the matrix
## the third sets the value of the inverse of the matrix
## the fourth gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        setMatrix <- function(y) {
                x <<- y
                i <<- NULL
        }
        getMatrix <- function() x
        setMatrixInv <- function(inverse) i <<- inverse
        getMatrixInv <- function() i
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setMatrixInv = setMatrixInv,
             getMatrixInv = getMatrixInv)
}


## the function "cacheSolve" retrieves the value of the matrix from the cache
## or calculates the inverse of the matrix if the value it's not present on the cache.
## in the first part there is an if test to check if the value is on the cache
## if not the second part calculates it and stores it on the cache.

cacheSolve <- function(x, ...) {
        i <- x$getMatrixInv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$getMatrix()
        i <- solve(data, ...)
        x$setMatrixInv(i)
        i
}
