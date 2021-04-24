## This function is used to inverse the invertible matrix. 

## makeCacheMatrix function can create a special matrix object.
## set() function create a special matrix
## get() function get the special matrix created by argument x or set()
## setSolve() function assign argument "solve" to m
## getSolve() function get "m"

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                ## Using "<<-" to managing variables in different environment.
                ## Double arrow operator can modify variables in parent levels.
                ## when excute 'x <<- y', the variable x changed in the parent levels(makeCacheMatrix
                ## enoironment.)
                x <<- y
                m <<- NULL
        }
        ## get() function using the principle of lexical scoping.
        ## x is not defined in the argument ,it is called free variable.
        ## so x is found in the environment where the function get() is defined (in this case, get() is defined in the /
        ## makeCacheMatrix environment.
        get <- function() x 
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get, 
                setSolve = setSolve, 
                getSolve = getSolve)
}


## cacheSolve function invert a singular matrix created by "c" function.
## x should be the object name of "makeCacheMatrix" function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## '$' is used to extract the element from a list.
        m <- x$getSolve()
        ## 'm' should be null when the orginal matrix (created by makeCacheMatrix) haven't been calculated
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        ## solve() function computes the inverse of a square matrix
        m <- solve(data)
        x$setSolve(m)
        m
}
