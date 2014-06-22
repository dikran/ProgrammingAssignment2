## Put comments here that give an overall description of what your
## functions do

## Creates an object in list form which is a matrix capable 
## of remembering its inverse.  The four items in the list 
## are 1. a function setting the value of the matrix
##     2. a function getting the value of the matrix
##     3. a function setting the value of the inverse
##     4. a function getting the value of the inverse
## In addition in this function there are two variable names
##     1. x, which is the underlying matrix of the list object
##     2. m, which is the inverse of this matrix (if already computed)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                # use of <<- : search for variable in parent environments  
                # assign values to x and m which don't exist in fcn set
                x <<- y
                m <<- NULL
        }

        # x exists in namespace of makeCacheMatrix
        get <- function() x

        # use <<- to assign value to m which doesn't exist in setInverse
        # but does exist in function makeCacheMatrix
        setInverse <- function(baz) m <<- baz

        # getInverse has access to this m in namespace of makeCacheMatrix
        getInverse <- function() m

        # this is the list object returned, described above
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix 'inv' that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # if we don't already have the inverse cached get the original matrix
        data <- x$get()
        # and invert it
        inv <- solve(data, ...)
        # set the inverse to the solution obtained
        x$setInverse(inv)
        # and return that solution
        inv
}


# test code : invert a simple matrix
x <-  matrix( c(1,0,2,1), nrow=2, ncol=2 )
myx <- makeCacheMatrix(x)
cacheSolve(myx)
cacheSolve(myx)
 