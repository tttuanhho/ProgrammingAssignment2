##Functions cache the inverse of a matrix

makeCacheMatrix <- function( m = matrix() ) {

    i <- NULL

    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Method the get the matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## Method to get the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Find the method to get the inverse 
    getInverse <- function() {
        ## Return property
        i
    }

    ## Return the list of methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ##Return the inverse if its initial set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from object
    data <- x$get()

    ## Calculate matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(m)

    ## Return value needed
    m
}
