## Author: Ieok Kok Wong
## Git Profile: https://github.com/kokwong


## makeCacheMatrix is a reference to function which has already loaded with 
## matrix X content into it

makeCacheMatrix <- function(x = matrix()) {
        solved <- NULL
        set <- function(y) {
                x <<- y
                solved <<- NULL
        }
        get <- function() x
        setsolved <- function(inversed) solved <<- inversed
        getsolved <- function() solved
        list(set = set, get = get,
             setsolved = setsolved,
             getsolved = getsolved)
}


## cacheSolve is a reference to function which can generate the Inverse of a 
## Matrix for the first time and look up the cached symbol table for subsequent
## executions

cacheSolve <- function(x, ...) {
        solved <- x$getsolved()
        if(!is.null(solved)) {
                message("getting cached inverse matrix data")
                return(solved)
        }
        data <- x$get()
        solved <- solve(data, ...)
        x$setsolved(solved)
        solved
}


## Example Usage
# A <- matrix( 
  # c(2, 4, 3, 1, 5, 7, 1, 2, 3), # the data elements 
  # nrow=3,              # number of rows 
  # ncol=3,              # number of columns 
  # byrow = TRUE)        # fill matrix by rows 
# J <- makeCacheMatrix(x = A)

## The first time is calculated
# cacheSolve(J)

## The second time onwards are retrieved from memory
# cacheSolve(J)
