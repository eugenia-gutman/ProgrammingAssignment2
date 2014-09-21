## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix or retrieves it from cache


## makeCacheMatrix is a list containing functions to 
## 1. Set the matrix
## 2. Get the matrix
## 3. Set the inverse matrix
## 4. Get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	  inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solved) inv <<- solved
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function calculates the inverse matrix of the special matrix created by makeCacheMatrix function.
## If inverse already calculated, it returns it; if not it calculates it, sets it it in cache and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	  inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv

}
