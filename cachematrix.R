# created both functions in the style of the example functions
# since it is kind of the same data structure / structural problem 
# as the example was, but a different computation
makeCacheMatrix <- function(x = matrix()) {
    xInv <- NULL
    set <- function(y) {
        x <<- y
        xInv <<- NULL
    }
    get <- function() x
    setInv <- function(inv) xInv <<- inv
    getInv <- function() xInv
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
}


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xInv <- x$getInv()
    if (!is.null(xInv)) {
        message("getting cached data")
        return(xInv)
    }
    matrix <- x$get()
    xInv <- solve(matrix, ...)
    x$setInv(xInv)
    xInv
}