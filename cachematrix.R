## This function takes in a matrix and returns a list of four objects
## which are, in essence, making a matrix object, retrieving the matrix object
## caching the matrix inverse and retriving the same

makeCacheMatrix <- function(x = matrix()) {

        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
}


## cacheSolve takes the an object X and checks whether or not there is a cached
## inverse matrix with values (assume matrix is invertible) and, if not available,
## calculates the inverse using solve - which actually is carrying out 
## x %*% b = 1 and finds b

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
        
        }
