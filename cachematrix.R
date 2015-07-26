## The two functions are used to create a special object that 
## stores a matrix and caches its inverse matrix. 
##
##
## The makeCacheMatrix function:
##
##  sets the value of the matrix
##  gets the value of the matrix
##  sets the value of the inverse matrix
##  gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## The cacheSolve function calculates the inverse of the special 
## "matrix" created with the above function makeCacheMatrix. 
## However, it first checks whether the inverse matrix has 
## already been calculated. If so, it gets the inverse matrix
## from the cache and skips the computation. Otherwise, 
## it calculates the inverse matrix of the given data (matrix) 
## and sets the value of the inverse matrix in the cache via the 
## setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
