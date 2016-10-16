## These functions give the inverse of a matrix, caching the 
## data, and retrieving data from the cache, where possible

## This first function sets the value of the vector, gets
## the value of the vector, sets the value of the inverse,
## and gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This second function calculates the inverse of the vector 
## created with the above function - but first it will check
## to see whether the inverse has already been calculated
## and cached. If so, it will display the cached value and
## skip the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(m)
        inv
}
