## These 2 functions allow us to cache matrix inversions

## makeCacheMatrix creates a special matrix, which is really
## a list containing functions to:
## set and get the value of the matrix
## set and get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
    set <- function(y) {
        #store matrix in parent environment
        x <<- y
        #invalidate matrix-inverse cache
        invx <<- NULL
    }
    get <- function() {
        x
    }
    setinv <- function(inv = matrix()) {
        #Store calculated inverse into cache (in parent environment)
        invx <<- inv
    }
    getinv <- function() {
        invx
    }
    list(
        set = set,
        get = get,
        setinv = setinv,
        getinv = getinv
    )
}

## The following function calculates the inverse of the
## special "matrix" created by the above function.
## It first checks to see if the inverse has already been
## calculated, returning "cached" version if so.
## Otherwise, the inverse is computed and assigned via setinv
cacheSolve <- function(x, ...) {
    # Retrieve cached matrix inverse (if it exists)
    invx <- x$getinv()
    if (!is.null(invx)) {
        # Cache is not empty, return cached inverse
        message("getting cached data")
        return(invx)
    }
    # Cache is empty, we'll need to compute the inverse
    data <- x$get()
    invx <- solve(data, ...)
    # Store inverse into cache
    x$setinv(invx)
    invx
}