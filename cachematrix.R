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

    # Inizialize the local variable 'inv' to NULL
    inv <- NULL

## The special assignment operator <<- is used to change the value
## associated with a variable, looking back in enclosing environmens
## and than replacing the value in the environment that contain the
## variable. 
    
    # set function: cache the matrix 'x'
    set <- function(y) {
        x <<- y # 
        inv <<- NULL # reset 'inv' to NULL
    }

    # get function: get cached 'x'
    get <- function() x  

    # setinv function: take the value of inversed 'x' and cache it as 'inv'
    setinv <- function(inverse){
        inv <<- inverse 
    }

    # getinv function: get cached 'inv'
    getinv <- function() {
        inv
    }

    # List of functions, so that they can be called by name with $
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

    # If not null (already evaluated), return value read from cache
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }

    # If null (never evaluated), assign the matrix to 'data'
    data <- x$get()

## solve(a, b, ...) solves the equation 'a %*% x = b' for 'x'. If
## missing, 'b' is taken to be an identity matrix and 'solve' will
## return the inverse of 'a'.

    # Solve the matrix as it's inverse in 'inv'
    inv <- solve(data)

    # Cache inv as calculated inverse of x
    x$setinv(inv)

    # Return a matrix that is the inverse of 'x'
    inv
}
