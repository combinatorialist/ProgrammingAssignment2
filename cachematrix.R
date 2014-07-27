## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Makes a list of setters and getters related to a matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## Finds the inverse of a matrix structure returned from the function makeCacheMatrix, or 
## simply returns it if it is already on the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cache data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setInverse(inv)
    inv
}
