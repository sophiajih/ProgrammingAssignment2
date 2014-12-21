## cacheMatrix and cacheSolve aim to cache time-consuming matrix inversion computations
## The goal is to cache the inverse of a matrix the first time it is computed
## and draw upon the cached value in future computations rather than compute it repeatedly

## makeCacheMatrix will create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {      # input x will be a matrix
    
    s <- NULL    #  s will be inverse matrix
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    get <- function() {x}   # returns the value of the original matrix
    
    setsolve <- function(solve)  { s <<- solve } # sets inverse matrix
    
    getsolve <- function() {s} # returns cached value of inverse matrix 
    
    list(set = set,
         get = get,                
         setsolve = setsolve,  
         getsolve = getsolve)  
    
}


## cacheSolve will compute the inverse of the "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {   
    s <- x$getsolve()               # accesses the object 'x' and gets the value of the inverse
    if(!is.null(s)) {              # if inverse was already cached ...
        
        message("getting cached data")  # ... send this message to the console
        return(s)                       # ... and return the inverse 
        
    }
    data <- x$get()        
    s <- solve(data, ...)   # calculate inverse if not yet calculated
    x$setsolve(s)           # store the calculated inverse 
    s               # return the inverse 
}
