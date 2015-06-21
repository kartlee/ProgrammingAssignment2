## Week2 - Caching the Inverse of a Square Matrix
##

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # @x: a square invertible matrix
    ## return: a list containing functions to
    ##   1. set the matrix
    ##   2. get the matrix
    ##   3. set the inverse
    ##   4. get the inverse
    ##   this list is used as the input to cacheSolve()
    
    # initialize the cache matrix
    cachematrix <- NULL
    
    set <- function (y) {
        # The matrix is same as before. So just return
        if(! (dim(x) == dim(y) && all(x==y))) {
            x <<- y
            cachematrix <<- NULL
        }
    }
    
    # get the value of the vector
    get <- function() x
    
    # set the inverse value of the matrix  and store in the cache
    setinverse <- function(inverse) cachematrix <<- inverse
    
    # get the inverted value of the matrix from cache
    getinverse <- function() cachematrix
    
    # return the created functions to the working environment
    list(set = set, get = get, setinverse= setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    ## @x: Square matrix created through makeCacheMatrix()
    ## return: inverse of the square matrix
    
    # compute inverse
    invmat <- x$getinverse()
    if (!is.null(invmat)) {
        message ("getting cached data")
        return(invmat)
    }
    
    # Calculate the inverse
    invmat <- solve(x$get(), ...)
    
    # cache the inverse
    x$setinverse(invmat)
    
    # return a matrix that is the inverse of 'x'
    invmat
}

## Unit test
##
testCacheInverse = function() {
    # Test 1
    print("Created a random distribution matrix of 100x100")
    temp = makeCacheMatrix(matrix(rnorm(100*100), 100, 100))
    startTime = Sys.time()
    cacheSolve(temp)
    durBeforeCache = Sys.time() - startTime
    cat("Calculating inverse for the first time took ", durBeforeCache,"\n")
    startTime = Sys.time()
    cacheSolve(temp)
    durAfterCache = Sys.time() - startTime
    cat("Calculating inverse for the second time took ", durAfterCache,"\n")
    if(durBeforeCache < durAfterCache) {
        print("Test to check caching inverse matrix failed")
    }

    # Test 2
    mat = matrix(rnorm(100*100), 100, 100)
    temp$set(mat)
    startTime = Sys.time()
    cacheSolve(temp)
    durAfterMatrixChange = Sys.time() - startTime
    cat("Calculating inverse with new matrix took ", durAfterMatrixChange,"\n")
    if(durAfterCache > durAfterMatrixChange) {
        print("Test to check if cache is invalidated failed")
    }
    
    # Test 3
    temp$set(mat)
    startTime = Sys.time()
    cacheSolve(temp)
    durAfterMatrixNotChanged = Sys.time() - startTime
    cat("Calculating inverse after matrix is set to same as before ", durAfterMatrixNotChanged,"\n")
    if(durAfterMatrixNotChanged > durAfterMatrixChange) {
        print("Test to check if cached is retained failed")
    }
}