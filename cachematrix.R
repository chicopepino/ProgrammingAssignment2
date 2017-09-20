## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.

## makeCacheMatrix is a list with four vectors.  Each vector is a function that 
## sets or gets the original or inverted matrix
## x is the orignal matrix
## m is the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL               #NULL the inverted matrix
    set <- function(y) {    #SET the matrix 
        x <<- y
        m <<- NULL
    }
    get <- function() x     #GET the matrix 
    setinv <- function(inv_matrix) m <<- inv_matrix   #SET the inv matrix  
    getinv <- function() m  #GET the inv matrix 
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
## x is the orignal matrix
## m is the inverted matrix

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m    ## Return a matrix that is the inverse of 'x'
}
