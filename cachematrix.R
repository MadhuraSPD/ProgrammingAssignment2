## Put comments here that give an overall description of what your
## functions do
##makeCacheMatrix: This function creates a "matrix" object that can cache its inverse.
##CacheSolve: This function computes the inverse of the "matrix" returned by makeCacheMatrix above. 
##

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
        #Reset value of m if it is declared earlier, and cache null value variables
        m <- NULL
        set <- function(y) {
        x <<- y
        m <<- NULL
        }
     #get value of matrix
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x' if it is calculated previously
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
       # Calculate inverse if not already done by calling value of x
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

#Check/test the code
mat1 <- makeCacheMatrix(x=matrix(1:4,2,2))
cacheSolve(mat1)
