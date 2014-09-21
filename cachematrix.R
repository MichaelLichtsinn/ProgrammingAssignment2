# makeCacheMatrix: This function creates a special "matrix" object that can 
# cache its inverse. This is done with the function solve() to calculate the inverse
# of the input matrix.

# This function returns a list of functions

makeCacheMatrix <- function(inputmatrix = matrix()) {
        # check if the input is a matrix. If not, it stops and gives a hint, that
        # the input must be a matrix.
        if (!is.matrix(inputmatrix)) {
                stop("inputmatrix must be a matrix")
        }
        
        inversematrix <- NULL
        
        # set the value of the matrix
        set <- function(y) {
                inputmatrix <<- y
                inversematrix <<- NULL
        }
        
        # get the value of the matrix
        get <- function() inputmatrix
        # set the value of the inverse matrix
        setinversematrix <- function(solve) inversematrix <<- solve
        # get the value of the inverse matrix
        getinversematrix <- function() inversematrix
        
        # returns the list of functions
        list(set = set, get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)
}

# cacheSolve: This function computes the inverse of the object returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), cachesolve retrieves the nverse from the cache.

cacheSolve <- function(inputmatrix, ...) {
        m <- inputmatrix$getinversematrix()
        # checks if the inversematrix is already cached
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        # if the inverse of the matrix is not already cached, it will compute
        # compute and cache it
        data <- inputmatrix$get()
        m <- solve(data, ...)
        inputmatrix$setinversematrix(m)
        m
}