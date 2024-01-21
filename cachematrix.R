###########################################################################
# Pair of functions for creating a cacheable matrix inverse method.
# makeCacheMatrix: creates a "matrix" object capable of holding its own inverse.  
# cacheInverse: calculates or retrieves the inverse of the matrix object
#               depending on whether it has been calculated before.
#
#  Also present is testCase() which tests various features of the above functions.
#######################################################################

# makeCacheMatrix
# - Creates a special version of the matrix.
# - This special version of the matrix is a list containing four 
#    functions that will allow cacheSolve to interact with it.
#      * set - modifies the matrix and sets it's inverse to NULL 
#              to force recalculation of the inverse next time cacheSolve() is run.
#      * get - retrieves the matrix itself
#      * setInverse - set the inverse for the matrix.
#      * getInverse - retrieves the inverse for the matrix.
#
# This method will create a new list object from the matrix and set
# its inverse (mInverse) to NULL. cacheSolve will utilize mInverse to determine
# whether the inverse has been calculated (minverse not NULL) or not (mInverse is NULL)
# if the inverse was already calculated it is just returned.  if not it will be calculated and saved
# in the matrix object

makeCacheMatrix <- function(x = matrix()) {
    mInverse <- NULL            
    
    # functions for interacting with the matrix.
    # get retrieves the matrix and set allows for the matrix to be changed 
    # with its inverse being cleared.
    set <- function(y) {          
        x <<- y
        mInverse <<- NULL
    }
    get <- function(y) { x }    


    #functions for interacting with inverse. set or retrieve the inverse.
    setInverse <- function(inv) { mInverse <<- inv}
    getInverse <- function() { mInverse } 
    
    # return the list version of the matrix
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

# CacheSolve(x,...)
# Takes our special list version of the matrix as input along with any desired parameters
# for the solve function. (...)
# When encountering our matrix object for the first time, 
# mInverse is NULL and the inverse must be calculated.
# Once assigned, subsequent calls to cacheSolve will retrieve the precalculated matrix inverse
# from the list object itself rather than calculating again
cacheSolve <- function(x, ...) {
    mInverse <- x$getInverse()              # try to retrieve the cached version of the inverse.
    if(!is.null(mInverse)) {                # do we have a cached version of the inverse?
        message("getting cached data")      # yep, return it!  
        return(mInverse)
    }    
    mat <- x$get()                          # nope:(, calculate it and save it for the future.
    
    # get matrix inverse if possible.  
    # requires determinant to be non-zero.
    if (det(mat) !=0) { 
    mInverse <- solve(mat, ...)
    } 
    else {
        message("Singular matrix, inverse does not exist.")
        mInverse = NA
    }
    x$setInverse(mInverse)
    mInverse    
}

# Test the various functions associated with makeCacheMatrix and CacheSolve.
testCase <- function() { 
    #create a blank cacheable matrix object.
    my_mat <- makeCacheMatrix()
    
    # Setup a test matrix
    m1 <- matrix( c(1,-1,0,-1,0,1,6,-2,-3),nrow=3,byrow=TRUE )
    
    # Assign it to our cacheable matrix object
    my_mat$set(m1)
    
    # Calculate the inverse. This should be calculated by the solve function.
    print("Finding the Inverse for the first time")
    print(cacheSolve(my_mat))

    # Calculate inverse again.  This should retrieve the stored version.
    print("Find the inverse a second time. Should come from the cache")
    print(cacheSolve(my_mat))
    
    # is this even the inverse? the product with the original should be a 3x3 identity matrix
    # or reallllly close to it.
    print("Checking the product of the inverse and the original matrix. Should be the identity matrix")
    print(round(my_mat$getInverse()   %*% m1,8))
    
    # Can we "uncache" the inverse.
    print("Removing cached inverse using setInverse()")
    my_mat$setInverse(NULL)
    
    # this should be recalculated using solve() again
    print("Finding the inverse after uncaching.")
    print(cacheSolve(my_mat))
    
    print("And again (should be cached this time)")
    print(cacheSolve(my_mat))    
    
    # try another matrix
    print("Setup another matrix using set()")
    m2<- matrix(rnorm(16),nrow =4 )
    my_mat$set(m2)
    print(my_mat$get())
    
    # find inverse (first should be calculated, second should be from cache.)
    print("Calculating inverse twice: first should be calculated, second should be obtained from cache.")
    print(cacheSolve(my_mat))
    print(cacheSolve(my_mat))
    
    # check inverse.
    print("Checking product...")
    print(round(my_mat$getInverse()   %*% m2,8))    
    
    # How about a singular matrix?
    print("Trying a singular matrix")
    m3 <- matrix(1:9,nrow=3)
    print(m3)
    my_mat$set(m3)
    cacheSolve(my_mat)
    print("Trying again..")
    cacheSolve(my_mat)    
}





