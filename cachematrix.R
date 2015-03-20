## These two functions make it possible to calculate the inverse of a matrix
## in a loop while avoiding to calculate the inverse of the same matrix again 
## but store it in cache and retrieve it from there.

## makeCacheMatrix prepares the data matrix for further processing by creating
## a list of functions and storing the matrix in one of these

makeCacheMatrix <- function(x = matrix()) {
    
    # set inverse to NULL
    inv <- NULL
    
    # function to read in a new data matrix
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    # get data matrix x
    get <- function(){x}
    ## set argument to inv
    setinv <- function(inverse){inv <<- inverse}
    # get inverse of matrix
    getinv <- function(){inv}
    # make list of functions
    list(set = set, get = get, 
         setinv = setinv,
         getinv = getinv)
    
}


## cacheSolve actually calculates the inverse of the matrix x that is the 
## argument of makeCacheMatrix. If it has been calculated before, it returns
## the inverse from cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # check if inverse has already been calculated 
    inv <- x$getinv()
    # if so, return value from cache
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    # if not, get matrix
    data <- x$get()
    # calculate inverse
    inv <- solve(data)
    # hand inverse to function to store it in cache
    x$setinv(inv)
    # show inverse of matrix
    inv
    
}
