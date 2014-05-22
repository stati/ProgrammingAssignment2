##  This is my implementation of programming assignment 2 of coursera R programming course.
##  These are couple of R functions that cache inverse of a matrix. 
##  These are helpful when inverse is to be calculated multiple times.

#makeCacheMatrix function takes a matrix as input and returns a list of functions to get/set the matrix or its inverse.
#Calling the set() function will invalidate the cached inverse.
#INPUT  : X - An invertible matrix whos inverse needs to be cached. Defaults to empty matrix.
#RETURN : A list of function to get/set matrix or it's inverse
makeCacheMatrix <- function(x = matrix()) {
    invX <- NULL
    set <- function(y) {
        x <<- y
        invX <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invX <<- inverse
    getinverse <- function() invX
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}


#cacheSolve function calculates inverse of a matrix created using makeCacheMatrix function.
#It returns cached value of inverted matrix, if inverse was previously calcualted.
#INPUT  : A matrix created using makeCacheMatrix
#RETURN : inverse of matrix passed to this function
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

##################USAGE#####################
# mymat <- makeCacheMatrix(matrix(data=c(1,2,3,4), nrow=2, ncol=2))
# myinv <- cacheSolve(mymat)
# myinv <- cacheSolve(mymat)
# getting cached data
# mymat$set(matrix(data=c(2,2,3,4), nrow=2, ncol=2))
# myinv <- cacheSolve(mymat)
# myinv <- cacheSolve(mymat)
# getting cached data
############################################