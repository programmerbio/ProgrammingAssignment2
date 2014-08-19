##makeVector creates a special "vector", which is really a list containing a function to
#1) set the value of the vector
#2) get the value of the vector
#3) set the value of the mean
#4) get the value of the mean

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

#calculates the mean of the special "vector" created with the above function. 
#However, it first checks to see if the mean has already been calculated. 
#If so, it gets the mean from the cache and skips the computation. 
#Otherwise, it calculates the mean of the data and 
#sets the value of the mean in the cache via the setmean function.
cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

# Your assignment is to write a pair of functions that cache the inverse of a matrix.

## Put comments here that give an overall description of what your
## functions do

##Make Matrix for practice
pracmatrix <- matrix(1:4, nrow=2, ncol=2)
pracmatrix
solve(pracmatrix)

## Write a short comment describing this function
#makeCacheMatrix creates a matrix and then an inverse matrix that will then will be stored for future retreival

makeCachematrix <- function(m = matrix()) {
     i <- NULL
     
     #Let's make a matrix
     set <- function(matrix) {
         m <<- matrix
         i <<- NULL
     }
     
     #retreve the matrix we just made
     get <- function() {
         m
     }
     
     #Sets the inverse of our matrix
     setinverse <- function(inverse) {
         i <<- inverse
     }
    
     ##retrieves the inverse we've just made
     getinverse <- function () {
         i
     }
     list (set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## Write a short comment describing this function
#cacheSolve will either compute an inverse of the matrix 
#or just retrieve the stored inverse from makeCachematrix function

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    
    ## retrieving cached inverse if it's there
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    
    ##retrieve our matrix
    data <- x$get()
    
    ## solve for the inverse matrix
    m <-  solve(data) 
    
    x$setinverse(m)
    
    ## spits out the matrix
    m
}

#check:
matrixObject <- makeCachematrix(pracmatrix)
cacheSolve(matrixObject)