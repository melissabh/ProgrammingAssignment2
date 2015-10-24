## File contains a function for creating a matrix object with a cacheable inverse and
## and function for getting the inverse from cache

## for the supplied matrix (or default empty one
## if one not specified) return a matrix object
## that has caching methods associated with it
## for the inverse
makeCacheMatrix <- function(x = matrix()) {
    myInverse <- NULL
    set <- function(m) {
        x <<- m
        myInverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) myInverse <<- inv
    getinverse <- function() myInverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## for the supplied matrix, return its cached inverse
## if it has been solved for before otherwise solve
## for its inverse, store it in cache, and return it
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}