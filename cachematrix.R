## Two functions to calculate the inverse of a matrix.
## If the inverse has already been calculated, the result will be read from
## memory instead of being calculated again.

## Usage:
## > a <- makeCacheMatrix(matrix)
## > cacheSolve(a)

## makeCacheMatrix:
## Inputs: x
## x: a matrix
## Outputs: a list of 4 functions: set, get, setinv, getinv
## set: Takes a single argument and sets the global variable x to be this.
## get: outputs the value assigned to global variable x.
## setinv: takes an input inv, and assigns it to the variable m.
## getinv: returns the value assigned to m.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
        }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## cacheSolve
## inputs: the list of functions output by makeCacheMatrix(matrix).
## outputs: the inverse of the matrix input to makeCacheMatrix.

## Assigns to local m the value assigned to global m.
## If this value is not null, then returns m as the output.
## If the value is null, then 
##     data is assigned to be the value of global x,
##     m is assigned to be the inverse of data,
##     and then the value of local m to global m,
##    before m is output as the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
