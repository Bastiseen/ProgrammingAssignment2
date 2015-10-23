## A pair of functions that cache the inverse of a matrix.

## The first function, makeCahcehMatrix creates a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matirx

makeCacheMatrix <- function(x = matrix()) {
## Empty inv
        inv <- NULL
## create the function set
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
## create the function get
        get <- function() x
## create the function set inverse
        setinverse <- function(inverse) inv <<- inverse
## create the function getinverse
        getinverse <- function() inv
## put the 4 functions in a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function, cacheSolve has for its aguments the outcome of the function makeCahcehMatrix.
## It returns the inverse of the matrix from the chache if its inverse was already computed.
## Otherwise, the function compute the inverse of the matrix.

cacheSolve <- function(x, ...) {
## Get the value in the cache
        inv <- x$getinverse()
## If the cahche already contain the inverse of the matrix, the corresponding value is printed. Otherwise:
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
## get the matrix        
        data <- x$get()
## compute the inverse of the matrix
        inv <- solve(data, ...)
## set the inverse of the matrix in the cache
        x$setinverse(inv)
## print the inverse of the matrix
        inv
}
