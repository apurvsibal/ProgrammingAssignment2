## Name: Apurv Sibal
## The function computes the inverse of matrix efficiently by using cache

## The function uses the <<- operator to create a image in a different environment

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(mean) m <<- mean
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function solves the problem by taking the inverse of the matrix using solve function

cacheSolve <- function(x, ...) {
	 m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
