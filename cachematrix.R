## We are creating two functions that will first create a special matrix to be inverted 
## and a secondary function that will check to see if that specific matrix has already 
## been inverted or not. If the matrix has not been inverted, the solve functionf for
## the matrix will executive.

## This function, makeCacheMatrix, creates a special "matrix, that contains the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverted matrix
## 4. get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve 
        getinverse <- function() inv
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks to see if there have already been a specific matrix
## that has already been inversed. If it hasn't been, the solve function wil
## execute to bring back the inverted matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return (inv)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(inv)
        inv
}
