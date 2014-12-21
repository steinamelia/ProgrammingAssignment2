## The following code contains a pair of functions that cache the
## inverse of a matrix to save computation time.  This code assumes
## that the matrix supplied is invertible.


## This function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y){
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x  ## Returns x
        set_inv <- function(inverse) inv_x<<-inverse
        get_inv <- function() inv_x  ## Returns inv_x
        list(set = set, 
             get = get, 
             set_inv = set_inv,
             get_inv = get_inv)  ## Returns the special "matrix"

}


## This function computes the inverse of a special "matrix" returned
## by makeCacheMatrix above.  If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv_x <- x$get_inv()
        if(!is.null(inv_x)){
                message("getting cached inverse")
                return(inv_x)
        }
        data <- x$get()
        inv_x <- solve(data, ...)
        x$set_inv(inv_x)
        inv_x

}