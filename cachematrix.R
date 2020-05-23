## These functions are designed to calculate the inverse 
#   of an invertible matrix, cache both the matrix and its inverse
# and provide a function for accessing that inverse.  The 

## 
#  Take a matrix as a parameter, initialize an object intended
#  to be the inverse matrix of the parameter as NULL, 
#   define 4 functions
#  which set and get the matrix and its inverse in a cache, 
#  return a list object comprised of the setter/getter
#  functions

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # create & set matrix in external environment & 
    # set inverse to NULL
    set <- function( yMat ) {
            x <<- yMat
            inv <<- NULL
    }
    # retrieve matrix
    get <- function() x
    # set the inverse in an external environment
    setinverse <- function( inverse ) inv <<- inverse
    # retrieve inverse
    getinverse <- function() inv
    list( set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse )
        
} # End of makeCacheMatrix


## cacheSolve takes a list object consisting of the 4 functions
# used to access the matrix and its inverse, retrieves the inverse
# if it already exists or calculates the inverse
# and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        # ( 1 ) If the inverse has been calculated, return it
        if( !is.null( inv ) ) {
                message( "getting cached inverse" )
                return( inv )
        }
        
        # (2) If the inverse has not been calculated, 
        # calculate it and return it
        matrix <- x$get()
        inv <- solve( matrix, ... )
        x$setinverse( inv )
        inv
}

