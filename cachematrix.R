## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix stores the matrix the inverse will be calculated for, 
## it resets the inv variable back to null for new matrices, 
## and stores the original matrix and the inverse matrix computed by the cacheSolve function in the parent environment.
## The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                                     ## The inverse is set to null, to initialise its value, so that it can later be used in the function.
        set <- function(y) {                            ## The set function is used to define new values of x and return the inv value to null, so that cacheSolve can re-compute the inverse
                x <<- y
                inv <<- NULL
        }
        get <- function() x                             ## The get function returns the value of the matrix to perform a check for the matrix of which we are getting the inverse
        setinverse <- function(inverse) inv <<- inverse ## The setinv function sets assigns the value of the inverse to the inv variable in the parent environment, so that it can be retrieved later via the makeCacheMatrix$getinv function
        getinverse <- function() inv                    ## The getinv function returns the inverse of the matrix x after cacheSolve has been ran
        list(set = set, get = get,                      ## The list order creates a list containing all the functions that were defined above. The functions are named so that they can be called with the $ operator afterwards.
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## cacheSolve actually uses the solve function to compute the inverse matrix of x, if the inv variable is null. 
## It then returns it to the makeCacheMatrix$setinverse function, so that it gets assigned to the parent environment and it can later be used to compare whether the inv
## is null or not.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
