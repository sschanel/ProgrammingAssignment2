## Wraps a matrix with a list of functions so the result of the solve() function
## can be saved, instead of calling it over and over again.

## makeCacheMatrix()
## Returns an interface to an object that can store the result of solve()

makeCacheMatrix <- function(x = matrix()) {
    ## cached inverse, only accessible via closure.
    inv <- NULL                 
    
    ## set() method.  Sets a new matrix value and clears the cache.
    set <- function(y) {
        x <<- y         
        inv <<- NULL
    }
    
    ## get() the matrix.
    get <- function() x

    ## setinverse() caches the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    ## getinverse() returns the cached inverse, or NULL
    getinverse <- function() inv 
    
    ## return as a list of functions
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)    
}


## Given an object returned by "makeCacheMatrix", either 1) return the cached
## result from the solve() function, or 2) call the solve() function, cache
## the result, and return the result.

cacheSolve <- function(cacheMatrix, ...) {
    
    ## Get the cached inverse
    inv <- cacheMatrix$getinverse()
    
    ## is it NULL?  If not, return the cached inverse.
    if (!is.null(inv)) {       
        return (inv)          
    }
    ## Get the actual matrix
    m <- cacheMatrix$get() 
    
    ## Get the inverse, assuming it is a square invertible matrix
    inv <- solve(m)
                               
    ## cache the result of solve() so the second time this gets called,
    ## we just return the already-solved inverse
    cacheMatrix$setinverse(inv)
                          
    ## return the inverse
    inv                  
}
