## The program helps in caching the inverse of matrix and return
## the same if the input matrix does not change.  

## makeCacheMatrix will input a matrix whose inverse has to be calculated
## It returns a list conatining functions to  - 
## 1. set the value of matrix, 2. get the value of matrix
## 3. set the inverse, 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<- function(y){
                x<<-y
                m<<-NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## cachSolve function returns the inverse of a square invertible matrix. 
## If the matrix inverse was already calculated,
## it will return the cached value and won't recalculate
## If the  determinant of matrix is zero (i.e. it is not invertible),
## it will return a message followed by a NULL value

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("Getting cached inverse matrix")
                return(m)
        }
        matrix<- x$get()
        if(det(matrix) == 0) {
                message("Determinant of matrix is zero. It is not invertible")
                return(NULL)
                
        }
        
        m <- solve(matrix)
        x$setinverse(m)
        m
}
