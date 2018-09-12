## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
        ## set the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get the matrix
        get <- function() x
        ## set inverse Matrix
        setInverse <- function(inverse) m <<- inverse
        ## get inverse Matrix
        getInverse <- function() m
        ## list of methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getInverse()
        ## return the inverse if set
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ##get data
        data <- x$get()
        ##multiplicate matrix
        m <- solve(data) %*% data
        ##set
        x$setInverse(m)
        ##return inverse Matrix
        m
}
