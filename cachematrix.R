## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## function makeCacheMatrix creates a list of functions which "contain" the data from the original-matrix by 
## using the data from the "frame-environment" because of lexical scoping ...



makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) { 
                x <<- y   
                m <<- NULL }
        get <- function() x
        settestmatrix <- function(matrice) m <<- matrice
        gettestmatrix <- function() m
        
        list(set = set, get = get,
             settestmatrix = settestmatrix,
             gettestmatrix = gettestmatrix) 
           
}


## Write a short comment describing this function
## function calculates the inverse of a matrix but if it was calculated before it returns value direct fdrom "cache" instead of recalculating

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$gettestmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$settestmatrix(m)
        m
}
