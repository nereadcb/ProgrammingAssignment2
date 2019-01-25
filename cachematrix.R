## Put comments here that give an overall description of what your
## functions do

## Caches the inverse of a give matrix in the parent environment

makeCacheMatrix <- function(x = matrix()) {
        ## initialize m
        m <- NULL
        ## set values
        set <- function(y = matrix()) {
                x <<- y
                m <<- NULL
        }
        ## retrieve values
        get <- function () x
        ## set with inverse matrix
        setinverse <- function(solve) m <<- solve
        ## retrieve inverse matrix
        getinverse <- function () m
        ## return list of all functions
        list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Returns the inverse of a matrix from cache if available or calculating it if not

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        ## checks if already exists
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## if it does not exist we get the matrix
        data <- x$get()
        ## and we calculate the inverse again
        m <- solve(data, ...)
        ## and set those values in the parent environment variable
        x$setinverse(m)
        ## returns the inverse of the matrix
        m
}
