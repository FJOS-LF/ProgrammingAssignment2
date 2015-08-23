## The two functions are used to create a special object that 
## stores a matrix and caches its inverse matrix. 
##
##
## Notice that, in what follows, one could use the names 
## "setinverse" and "getinverse". However, the names "setsolve" 
## and "getsolve" were used instead in order to emphasize the 
## native function "solve" in R. This choice agrees with the 
## name "cacheSolve" given in the assignment.
##
##
## The makeCacheMatrix function essentially creates a list 
## (thought as a "special matrix") containing a function to:
##
##  set the value of the matrix;
##  get the value of the matrix;
##  set the value of the inverse matrix; and 
##  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
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

## The cacheSolve function calculates the inverse of the 
## "special matrix" created with the above function 
## makeCacheMatrix. However, it first checks whether the
## inverse matrix has already been calculated. If so, it gets 
## the inverse matrix from the cache and skips the 
## computation. Otherwise, it calculates the inverse matrix of 
## the given data (matrix) and sets the value of the inverse 
## matrix in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix 'm' that is the inverse of 'x'
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
