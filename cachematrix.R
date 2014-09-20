## The following functions provide the ability to cache the inverse of a matrix to prevent
## multiple calculations of the inverse

## This function creates a special "matrix" that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setInverse <- function(inverseMatrix) im <<- inverseMatrix
    getInverse <- function() im
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function returns the inverse of a special matrix by retrieving the cached inverse or calculating it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    im <- x$getInverse()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data)
    x$setInverse(im)
    im
}
