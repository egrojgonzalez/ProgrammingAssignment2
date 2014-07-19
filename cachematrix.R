
## Jorge Gonzalez July 2104
## R Programming Assigment 2: Catching the inverse of a matrix
##----------------------------------------------------------------------
## makeCacheMatrix creates a special matrix object, and then cacheSolve 
## calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it, and not calculate it again.
## the x matrix must be square invertible matrix
## x will store the input matrix
## inv_x will store the cached inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        # Setter for the matrix
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        # Getter for the matrix
        get <- function() x
        
        ##Setter and Getter for the inverse
        setinverse<- function(inverse) inv_x <<-inverse
        getinverse <- function() inv_x
        
        # Return the matrix with our newly defined functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The function cacheSolve returns the inverse of x matrix created in the makeCacheMatrix function.
## If the cached inverse matrix has already calculated and the original matrix has not changed,
## the function cacheSolve retrieves it from the cache, if not, calculate again the inverse, caches, and returns it.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinverse()
        
        # If the inverse is already calculated, return it
        if (!is.null(inv_x)) {
                message("getting cached inverse matrix")
                return(inv_x)
        } else
                # The inverse is not yet calculated, so we calculate it
        {
                inv_x <- solve(x$get())
                # Cache the inverse and return it
                x$setinverse(inv_x)
                return(inv_x)
        }
}

# end of code 