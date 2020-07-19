## This function calculates the inverse of an array and caches it.

## The function creates an array, where its inverse can be cached.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inverse <<- solve
        getinv <- function() inverse
        list(set = set, get = get, setinv = setinv,
             getinv = getinv)
}


## Returns the inverse of a matrix.

cacheSolve <- function(x, ...) {
        inverse <- x$getinv()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        inverse
}

