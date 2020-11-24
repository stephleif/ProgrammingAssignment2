## Goal is to provide a cached version of a matrix so that it is easy
## and seamless to access the matrix inversion.  The actual inversion process will only
## be run when the original matrix is changed



## This function creates the cache of the matrix that is passed to it.
##  Note: This function does not do anything to the values of the matrix it stores

makeCacheMatrix <- function(x = matrix()) {
        mMatrix <- NULL
        ## function to set the value of the matrix
        set <- function(y) {
            x <<- y
            mMatrix <<- NULL
        }## end set
        ## function to get the value of the matrix
        get <- function() {
            return(x)
        }## end get
        
        ## function to set the inverse of the matrix
        setmatrix <- function(inver) {
            return(mMatrix <<- inver )
        }## end setmatrix
        getmatrix <- function() {
           return(mMatrix)
        }
        ## return from makeCacheMatrix a list
        return(list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix))
    }## end makeCacheMatrix


## the standard R function for matrix inverse is solve()
## this provides a solve that will use a cached value if available

## Usage
## > myMatrix_object <- makeCacheMatrix(m1)
## > cacheSolve(myMatrix_object)
## repeated calls to cacheSolve will then retrieve the cached values of myMatrix_object

## test examples are in the file testcachematrix.R

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        ## demo code of x$getmatrix() yields
        ## Error: $ operator is invalid for atomic vectors
        mMatrix <- x$getmatrix() ## get the cached value, will be null 
        
        if(!is.null(mMatrix)) {           ## if need to generate
            message("getting cached data")
            return(mMatrix)
        }
        data <- x$get()
        mMatrix <- solve(data, ...)
        x$setmatrix(mMatrix)
        return(mMatrix)
    }## end cacheSolve

