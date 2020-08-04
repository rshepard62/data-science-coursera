## Outputs a list of functions to be used by cacheSolve() and stores the input 
## matrix x within the makeCacheMatrix environment.

makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Outputs the inverse of the matrix x (The matrix x is stored within the
## makeCacheMatrix environment.) and caches the inverse of the matrix x, while
## using the output of the makeCacheMatrix function as the input. Subsequent
## calls of cacheSolve() outputs the cached value.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
