https://github.com/CPereir/Programming_Assignment_2/blob/master/cachematrix.R

https://github.com/Ku-Al/ProgrammingAssignment2/blob/master/cachematrix.R

## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it
## repeatedly.

## cachematrix.R is an R function to cache time-consuming matrix
## inversion. It assume that the matrix supplied is always
## invertible. It has 2 components: makeCacheMatrix and cacheSolve.

## makeCacheMatrix: This function creates a special "matrix" object
## which is really a list containing functions to:
## set the value of the Matrix
## get the value of the Matrix
## set the value of the Inverse of matrix
## get the value of the Inverse of matrix

makeCacheMatrix <- function(x = matrix()) {

    # Inizialize inv
    inv <- NULL

    # set function: 
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    # get function: 
    get <- function() x

    # setinv function:
    setinv <- function(solve) {
        inv <<- solve
    }

    # getinv function:
    getinv <- function() {
        inv
    }

    # list of functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    # Get inv from cache
    inv <- x$getinv()

    # If not null, return value read from cache
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }

    # If null, get inv from x
    data <- x$get()

    # solve(a, b, ...) solves the equation ‘a %*% x = b’ for ‘x’. If
    # missing, ‘b’ is taken to be an identity matrix and ‘solve’ will
    # return the inverse of ‘a’.
    inv <- solve(data)

    # Set inv as calculated inverse of x
    x$setinv(inv)

    # Return a matrix that is the inverse of 'x'
    inv
}
