## To use <<- operator to use lexical scoping in R and Cache the inverse of a matrix 
## to demonstrate how it functions.

## First we create "makeCacheMatrix" function to create a special "matrix" object that can 
## be used to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Now, we create "cacheSolve" function to return the inverse of the special matrix created using 
## "makeCacheMatrix" function. If the inverse is already calculated and the given matrix has not 
## been altered then the function will return the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matx <- x$get()
        inv <- solve(matx, ...)
        x$setInverse(inv)
        inv
}
