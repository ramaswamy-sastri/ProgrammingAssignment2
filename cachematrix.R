## Put comments here that give an overall description of what your
## functions do

## Below are two functions that create a special object that stores a matrix and caches the inverse of the matrix. 
## The caching avoids repeated executions of the inverse matrix by retrieving the result 
## from the cache if it already exists in the cache

## Sample execution
## > data <- makeCacheMatrix(matrix(c(4,2,7,6), nrow=2, ncol=2))
## > cacheSolve(data)
## above execution will calculate the inverse and store it in the cache
## > cacheSolve(data)
## above execution will retrieve data from cache as seen by the message "getting cached data"

## Write a short comment describing this function

## The makeCacheMatrix function creates a special matrix object that can cache its inverse
## set the matrix
## get the matrix
## set the inverse of the matrix
## get the inverse of the matrix

## assumption: the matrix x is a square invertible matrix
## Each call to makeCacheMatrix creates a "copy" of x in a new environment for the special matrix
## You can only change the data in the special matrix using the set function

makeCacheMatrix <- function(x = matrix()) {
        invmat <- NULL
        set = function(y) {
                x <<- y
                invmat <<- NULL
        }
        get = function() x
        setinv = function(inverse) invmat <<- inverse
        getinv = function() invmat
        
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

## Calculates the inverse of the matrix created with makeCacheMatrix. 
## if the inverse has already been calculated and the matrix has not changed, then returns the inverse from the cache
## otherwise uses the solve function to compute the inverse and store in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## check the cache first
        invmat = x$getinv()
        if(!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }
        
        ## nothing in cache; calculate the inverse using solve function
        data <- x$get()
        invmat = solve(data, ...)
        
        ##store calculated inverse in the cache
        x$setinv(invmat)
        
        return(invmat)
}
