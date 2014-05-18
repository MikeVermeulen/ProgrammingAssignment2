## Defines two functions. 
## The first function 
## returns a list of fuctions that get and set 
## the matrix arguement's values in the parent 
## environment.
## The second function
## retrieves a potentially expensive computation,
## the inverse of the arguement matrix, if it
## is cached. Otherwise the function calculates
## the inverse of the arguement matrix.


## returns a list of functions that get
## and set the Matrix Arguement and its
# Inverse.

makeCacheMatrix <- function(x = matrix()) {
        
                
        ## initialize the invM variable to NULL
        invM <- NULL
        
        ## set and get the the matrix arguement
        set <- function(y){
                ## `<<-` searches parent environment
                ## for an existing definition of the
                ## arguement variable 'x' and redefines
                ## its value. 
                
                x <<- y
                invM <<- NULL
        }
        
        
        ## get the matrix function
        get <- function() x
        
        ## ... get and set the inverses 
        setInverse <- function(MatrixInverse) invM <<- MatrixInverse
        getInverse <- function() invM
        
        
        ## return a list of functions
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)

}


## Computes or retrieves a cached
## matrix inverse

cacheSolve <- function(x, ...) {
       
                
        invM <- x$getInverse()
        
        
        if(!is.null(invM)) {
                message("getting cached data")
                
                ## return the cached inverse
                return(invM)
        }
        data <- x$get()
        invM <- solve(data, ...)
        x$setInverse(invM)
        
        ## return the calculated inverse
        invM
}
