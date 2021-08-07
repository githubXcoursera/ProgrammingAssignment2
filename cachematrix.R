## caches inverse matrix
makeCacheMatrix <- function( m = matrix() ) {

## inverse property is initialized
    i <- NULL
        ## sets matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }
    ## gets matrix
    get <- function() {
    	m
    }
    ## sets inv of matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
    ## gets inv of matrix
    getInverse <- function() {
        i
    }
    ## Returns list of methods above
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## computes the inverse of the special "matrix" returned by main code above 
## If the inverse has already been calculated, inverse from cache is retrieved

cacheSolve <- function(x, ...) {

    ## the inverse of x is returned to the matrix
    m <- x$getInverse()

    ## if it is set already, this code returns that matrix
    if( !is.null(m) ) {
            message("matrixing")
            return(m)
    }
    ## matrix is acquired from object
    data <- x$get()
    ## matrix multiplication is used to calculate it 
    m <- solve(data) %*% data
    ## object is set to inverse
    x$setInverse(m)
    ## matrix is returning
    m
}
