## makeCacheMatrix sets the parent environment, initializes variables, and defines 
## four functions via which we can manipulate variables
## and their values in the parent environment. 
## cacheSolve checks whether or not an inverse matrix stored in the parent 
## environment and if this is the case return it. If the function don't find
## the inverse matrix then it will calculate it and return the result while
## placing it (result) in parent environment. 
## cacheSolve2 is a cacheSolve with embedded set() option. Note that if want
## to change the basic matrix for which you calculate inverse one, you have to call
## set() function separately. In cacheSolve2 you can do it by just adding a new argument
## called "y", that should be the new matrix. All other characteristics remain the same.
## If you want just to get already stored inverse matrix, then do not specify the "y" argument. 

## The function below Sets the parent environment, initializes variables, and defines four functions

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y) {
                x <<- y
                inv <<- NULL
        }
        get <- function () x
        setinv <- function (inverse) inv <<- inverse
        getinv <- function () inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function below checks the parent environment for the inverse matrix and, if it not there,
## calculates it on it's own then places the result in the parent environment. Return inverse matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cashed data")
                inv
        } else {
                data <- x$get()
                inverse <- solve(data)
                x$setinv(inverse)
                inverse
        }
}

## cacheSolve with embedded set() option

cacheSolve2 <- function(x, y = matrix(), ...) {
        inv <- x$getinv()
        if (is.na(y)|identical(x$get, y)) {     #because of that row function returns warning message, it's ok.
                if (!is.null(inv)) {
                        message("getting cashed data")
                        inv
                } else {
                        data <- x$get()
                        inverse <- solve(data)
                        x$setinv(inverse)
                        inverse
                } 
        } else {
                x$set(y)
                inverse <- solve(y)
                x$setinv(inverse)
                inverse
        }
}
