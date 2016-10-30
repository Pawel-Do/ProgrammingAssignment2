## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly 

## This function creates a special "matrix" object that can cache its inverse
## The result of the function is a list containing a function to set/get value of the matrix
## set/get value of the inverse matrix (setInvmat/getinvmat)

makeCacheMatrix <- function(x = matrix()) {
        invmat    <- NULL
        set    <- function(y){
                   x <<- y
                   invmat <<- NULL
        }
        get <- function() x
        setInvmat <- function(inverse) invmat <<-inverse
        getInvmat <- function() invmat
        list(set = set, get=get,
             setInvmat = setInvmat, getInvmat= getInvmat) 
              ## List of objects is named so it will be possible 
              ## to access objects with $
    
}

## This function computes the inverse of the special "matrix" returned by  makeCacheMatrix  above. 
## If the inverse has already been calculated (and the matrix has not changed), then  cacheSolve  
## should retrieve the inverse from the cache.
## solve function is a built in function that allows inverting the invertible matrices
## function indicates when the result is pulled form cache by printing a message

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmat <-x$getInvmat()
        if(!is.null(invmat)){
          message("getting data from cache")
          return(invmat)
        }
        mat <- x$get()
        invmat <- solve(mat, ...)
        x$setInvmat(invmat)
        invmat
}
