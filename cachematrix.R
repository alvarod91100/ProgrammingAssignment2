## Two functions to cache a matrix inverse. First one creates the matrix itself. Second function computes the inverse on that special matrix. 
#Final result of running both is the inverse of the m matrix used in makeCacheMatrix as input.


## To create matrix that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
    i <- NULL
    
    #sets matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }
    
    #to get matrix
    get <- function() {
        return(m)
    }
    setInverse <- function(inverse) {
        i <<- inverse
    }
    
    getInverse <- function() {
        return(i)
    }
    #method list
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


cacheSolve <- function(x, ...) {

    m <- x$getInverse()

    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }
    
    #copies matrix
    data <- x$get()
    #gets inverse
    m <- solve(data)
    x$setInverse(m)
    
    #return inverse matrix
    m
}
