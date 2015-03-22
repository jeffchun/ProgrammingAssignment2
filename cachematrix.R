## The functions cache the value of the matrix inverse.
## When the inverse is needed again, it can be looked up in the cache
## rather than recomputing the value.


## "makeCacheMatrix" creates a special "list" containing a
## function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        set <- function(y){
                x <<- y
                cache <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) cache<<- solve
        getinverse <- function() cache
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## "cacheSolve" calculates the inverse of the special "list" created
## with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from
## the cache and skips the computation. Otherwise, it calculates the
## inverse of the data and sets the value of the inverse in the cache
## via the "setinverse" function.

cacheSolve <- function(x, ...) {
        cache <- x$getinverse()
        if(!is.null(cache)){
                message("getting cached data")
                return(cache)
        }
        data <- x$get()
        cache <- solve(data, ...)
        x$setinverse(cache)
        cache
}
