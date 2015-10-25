## The functions in this file cache the inverse of a matrix rather than computing it repeatedly
## For the purposes of this exercise we assume that supplied matrix are invertible.

## makeCacheMatrix creates a special matrix object that can cache its inverse.
## It creates a list of functions used to handle this caching
## The functions are as follows
## 1. set - sets the value of the matrix
## 2. get - gets the value of the matrix
## 3. set_inverse - sets the value of the inverse of the matrix
## 4. get_inverse - gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        matrix_cache <- NULL
        
        set <- function (y) {
                x <<- y
                matrix_cache <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse_value) {
                matrix_cache <<- inverse_value
        }
        get_inverse <- function()
                matrix_cache
        
        list(
                set = set,
                get = get,
                set_inverse = set_inverse,
                get_inverse = get_inverse
        )
        
}


## cacheSolve computes the inverse of the special "matrix" object returned by the above makeCacheMatrix function
## If the inverse has already been calculated and cached this function will return the inverse from the cache rather than recalculating

cacheSolve <- function(x, ...) {
        
        inverse_cache <- x$get_inverse()       ## Check if the inverse is already Cached
        if (!is.null(inverse_cache)) {
                message("getting cached data")
                return(inverse_cache)
        }
        matrix_cache <- x$get()                ## return the special matrix object in order to invert
        inverse_cache <- solve(matrix_cache)   ## Note this will error if the matrix is not invertible
        x$set_inverse(inverse_cache)
        inverse_cache
}
