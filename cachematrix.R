## there are 2 functions in this file makeCacheMatrix and cacheSolve


## This function stores the 4 function metadata in a list

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    	setmatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
	
    getmatrix <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(setmatrix=setmatrix, getmatrix=getmatrix, 
		 setinverse=setinverse, 
		 getinverse=getinverse)
}


## This function calls the different functions stored in a list 

cacheSolve <- function(x, ...) {
        ## If Inverse of matrix already exists then return that
	inv <- x$getinverse()
    	if(!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
    	}
    
    ##Else Read the matrix  
    data <- x$getmatrix()
    ## compute inverse matrix
    inv <- solve(data)
    ## Cache and return
    x$setinverse(inv)
    inv
}
