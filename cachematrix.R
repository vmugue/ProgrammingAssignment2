## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # when create new CachMatrix, old cache must be cleaned
    set <- function(y) { 
        x <<- y     # same when assign new matrix
        inv <<- NULL
    }
    #return matrix (use x$get to see matrix)
    get <- function() x
    # set new invertion
    setinv <- function(inv) inv <<- inv 
    # get existed inversion
    getinv <- function() inv
    
    # function returns object with functions which can be applied to x
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the invse of 'x'
    
    # get invertion from cache
    inv <- x$getinv()
    
    # if cache doesn't exist message will be shown
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # data - initial matrix, taken from cacheSolve object
    data <- x$get()
    #invertion calculation
    inv <- solve(data, ...)
    # write invertion in cache
    x$setinv(inv)
    #return invertion
    inv
}


