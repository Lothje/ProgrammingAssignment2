makeCacheMatrix <- function(x = matrix()) {
     ## This function creates a special "matrix" object that can cache its inverse
     ## set the value of the matrix
     ## get the value of the matrix
     ## set the value of the inverse of the matrix
     ## get the value of the inverse of the matrix
     
     ## Initial value of the cache of the inverse matrix
     inverseInCache <- NULL  
     
     ## ceation of the a matrix; Inverse is not calculated yet, so set to NULL
     set <- function(y) {
          x <<- y                
          inverseInCache <<- NULL
     }
     
     ## get the orginal matrix
     get <- function() x
     
     ## invert matrix and put it in cache
     setInverseMatrix <- function(inverse) inverseInCache <<- inverse
     
     ## get the inverse matrix from cache
     getInverseMatrix <- function() inverseInCache
     
     ## return the functions in this function
     list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}

cacheSolve <- function(x, ...) {
     
     ## Get the inverse matrix from x
     ## will be NULL in case inverse is not calculated yet
     inverseInCache <- x$getInverseMatrix()
     
     ## In case inverse matrix is available, get it from cache
     if(!is.null(inverseInCache)) {
          message("Inverse matrix from cache")
          return(inverseInCache)
     }
     
     ## get the original matrix data
     matrixData <- x$get()
     
     ## calculate the inverse of the original matrix
     inverseInCache <- solve(matrixData)
          
     ## and store inverse in cache
     x$setInverseMatrix(inverseInCache)
     
     ## print/return the inverse matrix (which is in cache)
     inverseInCache
}