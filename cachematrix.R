## 1.  `makeCacheMatrix`: This function creates a special "matrix" object
##      that can cache its inverse.
##2.    `cacheSolve`: This function computes the inverse of the special
##      "matrix" returned by `makeCacheMatrix` above. If the inverse has
##      already been calculated (and the matrix has not changed), then
##`     cacheSolve` should retrieve the inverse from the cache.



# The function "makeCacheMatrix" takes a square inversible matrix as argument and returns
# a list with 4 functions. This function is an adaptation of the makeVector function.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
# function "set" is setting value of matrix in another environment than current one.        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
# function "get" is getting value of stored matrix.
        get <- function() x
# function "setsolve" is setting value of inverted matrix in another environment than current one.
        setsolve <- function(solve) m <<- solve
# function "getsolve" is getting value of stored inverted matrix.        
        getsolve <- function() m
#list with 4 functions returned by makeCacheMatrix.
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



# The function "cacheSolve" takes the return of the makeCacheMatrix function as argument and 
# assumes that a square inversible matrix was provided to the makeCacheMatrix function.
# This function checks if an inverted square matrix was cached, if not the solve function
# is used to invert the square inversible matrix from the makeCacheMatrix function.
# This function is an adaptation of the cachemean function.

cacheSolve <- function(x) {
# checking if an inverted matrix was already stored in cache in x*$getsolve
        m <- x$getsolve()
# if yes, then warning message and function is returning inverted matrix from cache
        if(!is.null(m)) {
        message("getting cached data")
        return(m)
        }
#if not, getting the stored square inversible matrix and using the solve function
#to compute its inverted matrix then storing and returning it.
        data <- x$get()
        m <- solve(data)
        x$setsolve(m)
        m
        }
