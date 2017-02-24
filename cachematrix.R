## The functions can be used to store the value of a matrix and of its inverse matrix (if such exists).

## Function with a list of getter and setter functions. Takes a matrix as an argument. 

makeCacheMatrix <- function(x = matrix()) {
    solve <- NULL
    set <- function(y) {
        x <<- y
        solve <<- NULL
    }
    get <- function() x
    setsolve <- function(inverse) m <<- solve
    getsolve <- function() solve
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Function returns the inverse matrix stored in cache, or computes it if the value is not cached yet.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    solve <- x$getsolve()
    if(!is.null(solve)) {
        message("getting cached data")
        return(solve)
    }
    ## If the matrix is not square or is its det==0 -> 
    ## it is not invertible.
    matr <- x$get()
    isinversible<-class(try(solve <- solve(matr,..),silent=T))=="matrix"
    if(!isinversible) {
        message("The matrix is not inversible!")
        return(NULL)
    }
    else {
        x$setsolve(solve)
        return(solve)
    }

}
