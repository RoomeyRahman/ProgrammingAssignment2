## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        
        setMatrix <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        
        getMatrix <- function() x
        setinvMatrix <- function(inverse) invMatrix <<- inverse
        getinvMatrix <- function() invMatrix
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setinvMatrix = setinvMatrix,
             getinvMatrix = getinvMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        invMatrix <- x$getinvMatrix()
        if(!is.null(invMatrix)) {
                message("getting cached Invertible Matrix")
                return(invMatrix)
        }
        matrixdata <- x$getMatrix()
        invMatrix <- solve(matrixdata, ...)
        x$setinvMatrix(invMatrix)
        return(invMatrix)
}
