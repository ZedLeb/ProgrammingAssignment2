## The code below based on example given in the instructions for
## Programming Assignment 2: Lexical Scoping
## The script contains a pair of functions that cache the inverse
## of an invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
        ## This function creates a special "matrix" object that can 
        ## cache the inverse of an invertible matrix.
        
        # Variable for the cached value of the inverse of x
        inv <- NULL  
        
        # Function to set the replacement matrix
        set <- function(newx) {
                # note super-assignment operator used to set the x and 
                # i variables to the enclosing environment.
                x <<- newx
                inv <<- NULL
        }
        
        # Function to return the matrix x
        get <- function() {
                # Return the matrix
                x
        }
        
        
        # Function to store the supplied inverse of the matrix
        setinverse <- function(newinv) {
                inv <<- newinv
        }
       
        
        # Function that returns the cached inverse of x
        getinverse <- function() {
                # Return the inverse
                inv
        }
        
        # Return a list of all functions with names
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
        ##This function computes the inverse of makeCacheMatrix.
        ##If the inverse has already been calculated (and the matrix has not 
        ##changed), then the cacheSolve() should retrieve the inverse from the cache.
        
        # Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        #If the inverse has already been computed, use that.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # else (as inv IS null) call get() from makeCacheMatrix
        # and assign the matrix to data
        data <- x$get()
        
        # Compute the inverse of data using matrix multiplication
        inv <- solve(data, ...)
        
        # call setinverse() from makeCacheMatrix and
        # pass the newly created inverse
        x$setinverse(inv) 
        
        ## return the newly created inverse
        inv
}