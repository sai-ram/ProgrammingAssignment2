## These two functions create a special object that stores a matrix and caches its inverse 

## makeCacheMatrix creates a special "Matrix" which is really a list containing 4 functions 
## 1) To set the value of the matrix
## 2) To get the value of the matrix
## 3) Set the value of the inverse of the matrix
## 4) Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function, cacheSolve, calculates the inverse of the special "Matrix" 
## created with the previous function. Before calculating, it first checks 
## to see if the inverse has already been calculated. If it has been, it 
## gets the inverse from the cache and skips the computation. If not , it 
## calculates the inverse of the matrix and sets the value of the inverse 
## in the cache using the setinv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
            message("Getting cached inverse of matrix")
            return(inv)
        }
        message("Calculating inverse of matrix")
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
