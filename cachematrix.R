## The makeCacheMatrix and cacheSolve  functions allow the user to create a special matrix
## structure that it is able to cache their inverse avoiding costly computation if the 
## the inverse have already be calculated

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the value of the inverse
## 4 get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        # s will store the cache inverse matrix
        s <- NULL
        
        # Setter and Getter for the matrix
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        
        # Setter and Getter for the inverse
        setInverse <- function(inverse) s <<- inverse
        getInverse <- function() s
        
        # Return the matrix with newly defined functions
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the 
## inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getInverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        # The inverse is not yet calculated, so we caclulate it and store it
        data <- x$get()
        s <- solve(data, ...)
        x$setInverse(s)
        s
}
