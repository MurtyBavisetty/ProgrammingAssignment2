##
## Prgramming Assignment: 2
##
## makeChaeMatrix is used to set, get the square matrix 
## and also set and get the inverse of the given matrix.
## It saves the repeatedly used matrix (x) and its inverse (m) in the cache and retrieves
## them when needed. This saves the computational time for solving the inverse of the 
## same matrix once and saves in the cache.
## Note: The matrix inversion is a time consuming operation for large matrices
##
makeCacheMatrix <- function(x = matrix()) {

	    ## Set the cached value of the inverse of the matrix to NULL as the actual inverse is
	    ## set in the setSolve function below

        m <- NULL

        ## The following function is use to set the matrix (y)
        set <- function(y) {
                
                ## Save the new matrix (y)  into variable x in global domain
                x <<- y

                ## set the inverse of the matrix (y) to NULL as we are setting the matrix here.
                m <<- NULL    
        }

        ## The following get function is used to get the stored matrix (x) from global environment
        get <- function() x
        
        ## The following function saves the inverse of the matrix (inv_matrix) to m 
        ## (in global env). 

        setSolve <- function(inv_matrix) m <<- inv_matrix
        
        ## Get the inverse of the matrix from global environment 
        ## This could return either NULL or actual inverse of the matrix (m) if it has 
        ## been already computed and saved.
        getSolve <- function() m
        
        ## The following is the list of all the functions defined in this function object
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## Write a short comment describing this function
## This function returns the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache (global env).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       m <- x$getSolve()

       ## The following condition is true if the matrix inverse (m) is already saved previously 
       ## to the cache

        if(!is.null(m)) {
                message("getting cached inverse of the matrix ")
                return(m)
        }

        ## We do the following if the cached inverse is not available
        ## Get the matrix to be inverted from the cache and save it locally. 
        data <- x$get()
        
        ## Compute the inverse of the matrix (data) first time using Solve function in R
        ## and save it to m locally
        ## The matrix (data) is assumed to be invertible and hence no checks are made.
        m <- solve(data, ...)
  
        ## Cache m (inverse of the matrix(data)) using the setSolve function defined in 
        ## makeCacheMatrix
        ##The inv_matrix is obtained by Solve function in R for the given 
        ## non-singular square matrix (i.e. square matrix for which inverse exists)
        x$setSolve(m)
        
        ## Return the newly computed matrix inverse
        m
}
