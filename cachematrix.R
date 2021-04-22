## This function is used to inverse the invertible matrix. 

## makeCacheMatrix function can create a special matrix object.
## set() function create a special matrix
## get() function get the special matrix created by argument x or set()
## setSolve() function assign argument "solve" to m
## getSolve() function get "m"

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get, 
                setSolve = setSolve, 
                getSolve = getSolve)
}


## cacheSolve function invert a singular matrix created by "makeCacheMatrix" function.
## x should be the object name of "makeCacheMatrix" function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setSolve(m)
        m
}
