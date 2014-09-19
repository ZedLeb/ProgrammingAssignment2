##The code below based on example given in the instructions for
##Programming Assignment 2: Lexical Scoping


##This function creates a special "matrix" object that can 
##cache the inverse of an invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
        
        # set variable i (the inverse) to NULL
        i <- NULL  
        
        # use super-assignment operator <<- to set the x and 
        # i variables to the enclosing environment.
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        # return the value of x
        get <- function() x
        
        # set i in the local funtion (the argument inverse)
        # to the value of i found in the enclosing environment
        # superassign i to the enclosing environment
        ## NOTE: I don't understand why or how setinverse runs
        ## before i is superassigned...
        setinverse <- function(inverse) i <<- inverse
        
        # return the value of i
        getinverse <- function() i
        
        # create a list vector of functions with names
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##This function computes the inverse of makeCacheMatrix.
##If the inverse has already been calculated (and the matrix has not 
##changed), then the cacheSolve() should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        # call getinverse() from makeCacheMatrix and
        # assign to i 
        i <- x$getinverse()
        
        # if() checks that i is NOT null and prints message that
        # cached data will be used
        # return the cached inverse
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # else (as i IS null) call get() from makeCacheMatrix
        # and assign the value to data
        data <- x$get()
        
        # calculate the inverse of data with solve() function 
        i <- solve(data, ...)
        
        # call setinverse() from makeCacheMatrix and
        # pass the newly created inverse
        x$setinverse(i) 
        
        ## return the newly created inverse
        i
}
