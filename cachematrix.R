## Compute the inverse of a matrix: if the inverse has already been cached, retrieve
## the inverse from the cache instead of performing the raw calculation.

## Create a special "matrix" object to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL ## start with an empty value for the inverse
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x ## store the value of the original matrix
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)  ##return a list that can be called upon by cacheSolve 
}


## Calculate the inverse of a matrix, first checking the cache.  If the result has been cached, 
## retrieve the value from the cache.  If nothing has been cached, compute the inverse via solve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) { ##retrieve from cache if inverse is already stored
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...) ##compute inverse from solve if the inverse hasn't been cached yet
    x$setinverse(i)
    i  ##return the inverse matrix
}
