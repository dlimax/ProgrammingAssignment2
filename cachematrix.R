## Creates functions that provides a mechanism of calculating the inverse
## matrix of a given matrix using caching.

##  creates a special "vector", which is a list containing a function to
## set the value of the matrix; get the value of the matrix; 
## set the value of the inverse matrix; get the value of the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Uses the special vector of the above function to calculate
## the inverse matrix, only when it is necessary, and 
## return it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <-solve(data, ...)
        x$setInverse(m)
        m
}
