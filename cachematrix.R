## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
makeCacheMatrix <- function(x = matrix()) {
                Cacheinv <- list()
                set <- function(y){
                                x <<- y
                } # define function set in order to change the variable x globally.
                get <- function() x
                setInvMatrix <- function(inverse = matrix()){
                                Cacheinv$x <<- inverse
                } 
                getInvMatrix <- function(x) Cacheinv$x
                list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
                inv <- x$getInvMatrix()
                if(!is.null(inv)){
                                message("getting cached inverse of the matrix")
                                return(inv)
                }
                data <- x$get()
                inv <- solve(data, ...)
                MC$setInvMatrix(inv)
                return(inv)
}

# This is the example of these two functions:
# x <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3, ncol = 3)
# y <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3, ncol = 3)
# MC <- makeCacheMatrix(x)
# cacheSolve(MC) # I can use this to solve x
# MC$set(y) # We can change the variable by set()
# cacheSolve(MC) # we can solve this matrix by cache