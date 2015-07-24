## below are 2 functions: 
## 1. makeCacheMatrix : This function creates a special "matrix" object that caches its inverse.
## 2. cacheSolve : This function computes the inverse of the special "matrix" returned by  makeCacheMatrix  above. If the inverse has already been calculated (and the matrix has not changed), then  cacheSolve  should retrieve the inverse from the cache.

## About makeCacheMatrix:
## makeCacheMAtrix creates an object  which is really a list containing functions to 1.set the value of the vector
## 1. set the matrix 2. get the matrix 3. set the value of Inversed Matrix
## 4.get the value of inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        InverseMatrix <- NULL
        
        set <- function(y) {
                x <<- y
                InverseMatrix <<- NULL
        }
        
        get <- function() x
        setInverseMatrix <- function(IM) InverseMatrix <<- IM
        getInverseMatrix <- function() InverseMatrix
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## The following function calculates the inverse matrix for the special "matrix" object 
## created with the above function. It first checks to see if the InverseMatrix has already 
## been calculated. If so, it  gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates reverse matrix and sets the Inversed Matrix in the cache via the  
## setInverseMatrix  function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        IN<-x$getInverseMatrix()
        
        if(!is.null(IN)) {
                message("getting cached data")
                return(IN)
        }
        
        data <- x$get()
        IN <- solve(data)
        x$setInverseMatrix(IN)
        IN
}
