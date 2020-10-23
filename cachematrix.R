## get a matrix and cache the inverse for the giving matrix


## Take a square matrix and return a list of four functions(set, get, setInverse, getInverse)
## This four functions are the attributes of the matrix that the function makeCacheMatrix() makes
## this four functions/attributes allow the user to modify the matrix.

makeCacheMatrix <- function(x = matrix()) {
        x_inverse <- NULL
        set <- function(y) {    
                x <<- y
                x_inverse <<- NULL
        }
        
        get <- function() x
        setInverse <- function(inverseX) x_inverse <<- inverseX
        getInverse <- function() x_inverse
        list(set = set, get = get,
             setInverse = setInverse, getInverse = getInverse)
}


## Take an makeCacheMatriix object and test if there is inverse for that matrix in the cache memory
## if so it prints a message indicate that the inverse matrix already exist and return the inverse matrix
## otherwise calculate the inverse matrix and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_inverse <- x$getInverse()
        if (!is.null(x_inverse)) {
                message("getting cached matrix")
                return(x_inverse)
        }
        
        data <- x$get()
        x_inverse <- solve(data, ...)
        x$setInverse(x_inverse)
        x_inverse
}
