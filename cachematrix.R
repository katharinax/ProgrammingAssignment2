
##############################################################
#
# Date created: 16May15
# Course: Coursera rprog
# Instructor: Roger Peng
# Student name: Katharina Huang
# Purpose: Caching the inverse of a matrix x
# Assumptions: x is a square invertible matrix
#
##############################################################

# The makeCacheMatrix function stores a list of four functions which :
# 1. sets the value of the matrix
# 2. gets the value of the matrix
# 3. sets the value of the inverse of the matrix
# 4. gets the value of the inverse of the matrix
# Note: the <<- operator assigns values to objects in a different environment

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# The cacheSolve function returns the inverse of the matrix x
# It passes in the makeCacheMatrix function, and
# checks for existing inverse of the matrix

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

## Testing the functions
# 
# a <- matrix(c(1, 5, 5, 5, 1, 5, 5, 5, 1), nrow = 3)
# 
# cacheSolve(makeCacheMatrix(a))
# cacheSolve(makeCacheMatrix(a))
# 
# cache.of.a <- makeCacheMatrix(a)
# cacheSolve(cache.of.a)
# cacheSolve(cache.of.a)

