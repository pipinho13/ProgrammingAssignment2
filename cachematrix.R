
makeCacheMatrix <- function(x = matrix()) {

        xinverse <- NULL
        set <- function(y) {
                x <<- y
                xinverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) xinverse <<- inverse
        getinverse <- function() xinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
### then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

xinverse <- x$getinverse()
        if(!is.null(xinverse)) {
                message("Getting cached data")
                return(xinverse)
        }
        message("Calculating")
        data <- x$get()
        xinverse <- solve(data, ...)
        x$setinverse(xinverse)
        xinverse
        ## Return a matrix that is the inverse of 'x'
}
