## These functions look up for the inverse of a matrix in the cache and return 
## its value. However, if the inverse is not found, the functions compute the 
## inverse.

## This function creates a special "matrix" which is really a list containing 
## functions to: set the value of a matrix, get the value of the matrix, set the 
## value of its inverse and get the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL        
        }
        get <- function() x
        setinverse <- function(sol) i <<- sol
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## This other function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the data and caches 
## the value of the inverse via the setinverse function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
