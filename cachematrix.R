## 1.  `makeCacheMatrix`: This function creates a special "matrix" object
##      that can cache its inverse.
##2.    `cacheSolve`: This function computes the inverse of the special
##      "matrix" returned by `makeCacheMatrix` above. If the inverse has
##      already been calculated (and the matrix has not changed), then
##`     cacheSolve` should retrieve the inverse from the cache.
##      functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mymatrix = matrix()) {
        m <- NULL
        
        set <- function(y) {
                mymatrix <<- y
                m <<- NULL
        }
        get <- function() mymatrix
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(mymatrix, ...) {
        ## Return a matrix that is the inverse of 'mymatrix'
        m <- mymatrix$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- mymatrix$get()
        m <- solve(data, ...)
        mymatrix$setsolve(m)
        m
        }
