## Caching the inverse of a Matrix: 
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly (there are also alternatives to matrix inversion that we will not discuss or implement here).

## The first function "makeCacheMatrix" creates a special "matrix", which is really a set containing a function to 
# 1. Set the matrix
# 2. Get the matrix
# 3. Set the inverse matrix
# 4. Get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    # initialize the stored inverse matrix to NULL
    invMatrix <- NULL

    # set value of the matrix
    set <- function(y){
        x << y 
        invMatrix << NULL # matrix has changed, reassign to NULL
    }
    
    # get the value of the matrix
    get <- function() x

    # set the inverse of the matrix: Solve function in R does this
    setInvMatrix <- function(solve) invMatrix<<-solve

    # get the inverse of the matrix
    getInvMatrix <- function() invMatrix

    # return the list containing all the functions defined above
    list(set = set, 
         get = get, 
         setInvMatrix = setInvMatrix, 
         getInvMatrix = getInvMatrix)
}


## This function computes the inverse of the special matrix returned by the makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    # get the inverse of the matrix
    invMatrix <- x$getInvMatrix()

    # if inverse exists, check if it was already cached
    # if cached, return the inverse matrix cached
    if(!is.null(invMatrix)){
        message("getting cached data") # display that the cached data is retrived
        return(invMatrix)
    }

    # if not cached, get the matrix
    data <- x$get()
    
    # compute the inverse of the matrix
    invMatrix <- solve(data, ...)

    # cache the inverse matrix
    x$setInvMat(invMatrix)

    # return the inverse matrix
    invMatrix
}

