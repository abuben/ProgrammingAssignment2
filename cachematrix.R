## makeCacheMatrix and cacheSolve compute and cache the inverse
## of matrix `x' to minimize costly computation time during
## inversion of matrices

## makeCacheMatrix creates the inverse of matrix 'x' and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of matrix 'x'
## Inverse is retrieved from cache if already computed and not modified
## Otherwise inverse is calculated and then stored in cache

cacheSolve <- function(x, ...) {
        inv = x$get()
        

        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv = solve(data, ...)
        x$setinv(inv)
        
        inv
}
