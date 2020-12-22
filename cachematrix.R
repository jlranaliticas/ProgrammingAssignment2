## makeCacheMatrix 
##      PURPOSE: The following function initializes a cache to hold the
##               inverse of an invertible matrix.  It's primary function is to
##               set up the placeholder for the inverted matrix and define the
##               functions which "cacheSolve" will use to retrieve and store
##               the inverted function.

## cacheSolve 
##      PURPOSE:  The following function returns the inverse of an matrix.  
##                (1) First the function checks to see if the inverted matrix 
##                    has already been created (this would have been done by 
##                    the makeCacheMatrix function)  If yes, then the inverted 
##                    matrix is returned. Note:  1st time inverting a matrix, 
##                    it will not be in the cache and message is displayed.
##                (2) If there was no cached inverted matrix, then the 
##                    inverse is calculated, cached and stored as a global
##                    variable.

## EXECUTION OVERVIEW:  The 2 functions work in tandem.  'makeCacheMatrix' is
##                initially executed to establish the environment.  'cacheSolve'
##                then generates the inverted matrix and calls the 'setm_inv'
##                to store the inverted matrix in the cache.
## ############################################################################

## makeCacheMatrix 
##      PURPOSE: The following function initializes a cache to hold the
##               inverse of an invertible matrix.  
##      INPUT:  Invertible matrix 'x'
##      OUTPUT: List containing functions which will get the original matrix and 
##              functions to get/save inverted matrix 

makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL
        set <- function(y = matrix) {
                x <<- y
                m_inv <<- NULL
        }
        get <- function() x
        setm_inv <- function(solve) m_inv <<- solve
        getm_inv <- function()m_inv
        list(set = set,
             get = get,
             setm_inv = setm_inv,
             getm_inv = getm_inv)

}


## cacheSolve:  The following function returns the inverse of an matrix by 
##              first checking the cache.
##      INPUT:  List from 'makeCacheMatrix' containing point to matrix to be 
##              inverted and functions to cache the inverted matrix
##      OUTPUT: Inverted matrix which is also cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m_inv <- x$getm_inv()   
        if(!is.null(m_inv)) {    
                message("getting cached data...")
                return(m_inv)
        }
        data <- x$get()
        m_inv <- solve(data)
        x$setm_inv(m_inv)
        m_inv
}
