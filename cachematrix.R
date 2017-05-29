## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix initializes 'm' (holding spot for inverted matrix) to NULL and creates a list of functions
## that will load an inverted matrix from "cache" or calculate, store in parent environment and return the inverted matrix

## Write a short comment describing this function
## makeCacheMatrix sets the m object to NULL
## makeCacheMatrix returns a list of functions to be used by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y #Assigns Matrix to 'x' in the parent envirinment
                m <<- NULL #Assigns NULL to 'm' in the parent environment
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve #Assigns 'm' in the parent environment
        getsolve <- function() m #Gets 'm' in the parent environment
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve) # Create List with named elements of functions
}


## Write a short comment describing this function
## Calls getsolve() to check if NULL. If its not NULLL, the function will print "getting cached data" and return value stored.
## If there is not value stored, then get the matrix, solve the matrix, then call a function to set solution 
## to the parent environment. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
