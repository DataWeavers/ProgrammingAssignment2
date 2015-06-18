## This caches the inverse of any invertible matrix
## and returns the cache value every time

## This function gets any invertible matrix and
## returns a list of four functionss as bellow

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvers <- function(invers) m <<- invers
        getinvers <- function() m
        list(set = set, get = get,
             setinvers = setinvers,
             getinvers = getinvers)
}


## This function examins if the inverse of the
## matrix already exists. If not, it calculates
## the inverse and returns the value

cacheSolve <- function(x, ...) {
        m <- x$getinvers()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvers(m)
        m
}
