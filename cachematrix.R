## Example usage of the following two functions:
##
## x <- matrix(1:4, 2, 2)
## xx <- makeCacheMatrix(x)
## y <- cacheSolve(xx)
## 

##------------------------------------------------------------------------------
## This function creates a special "matrix" object that can cache its inverse
##
## RETURN - list of functions
## 
makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize variable
    inv <- NULL
    
    ## Object setter 
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    ## Object getter
    get <- function() x
    
    ## Set inverse matrix
    setInverse <- function(solve) inv <<- solve
    
    ## Get inverse matrix
    getInverse <- function() inv
    
    ## Return the list
    list( set = set, 
          get = get, 
          setInverse = setInverse, 
          getInverse = getInverse )
}


##------------------------------------------------------------------------------
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.
##
## RETURN - a matrix that is the inverse of 'x'
##
cacheSolve <- function(x, ...) {
  
    ## Get existing inverse matrix
    inv <- x$getInverse()
    
    ## Have we got valid inverse matrix
    if( !is.null(inv) ) {
      
        ## Yes, return the inverse matrix
        return(inv)
    }
    
    ## Get original matrix
    data <- x$get()
    
    ## Inverse the matrix
    inv <- solve(data, ...)
  
    ## Cache the inverse matrix
    x$setInverse(inv)
    
    ## Return inverse matrix
    inv
}


