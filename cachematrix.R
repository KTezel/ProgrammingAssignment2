## makecacheMatrix creates a special matrix object that can cache its
## inverse


makeCacheMatrix <- function(x = matrix()) {
        ## inverse variable
        invMat <- NULL
        
        ## set function
        set <- function(y) {
                x <<- y
                invMat <<- NULL
        }
        
        ## get function
        get <- function() x
        
        ## setInverse and getInverse functions
        setInverse <- function(inverse) invMat <<- inverse
        getInverse <- function() invMat
        
        ## assign functions 
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve will create the inverse of the matrix
## for this function to work, matrix has to be invertible
## if the inverse has been calculated, the the cacheSolve
## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMat <- x$getInverse()
        if(!is.null(invMat)) {
                message("getting cached data")
                return(invMat)
        }
        data <- x$get()
        invMat <- solve(data, ...)
        x$setInverse(invMat)
        invMat
}

