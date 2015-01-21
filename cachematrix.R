## compute the inverse of a matrix, then cache the inverse for requirement later
## assume that the matrix supplied is always invertible

## create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL ## if the matrix change, clear cache
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## compute the inverse of the "matrix" returned by 'makeCacheMatrix'
## retrieve the inverse from the cache or calculate inverse when cache is empty
cacheSolve <- function(x) {
    i <- x$getinverse()    
    ##cache is not null, return inverse retrieved from cache
    if(!is.null(i)){
        message("get the cache inverse")
        return (i)
    }
    ##cache is null, calculate inverse, then store
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    ## Return a matrix that is the inverse of 'x'
    i
}
