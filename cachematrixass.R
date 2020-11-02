libarary(MASS)
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function()x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() {
                        inver<-getinv(x)
                        inver%*%x  #to obtain inverse of the matrix
                        }
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
##this is used to get the cache data

cachesolve <- function(x, ...) ##GETS THE CACHE DATA
{
        inv<-x$getinv()
        if(!is.null(inv)){
                message("getting the cached data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinv(inv)
        inv  ##returns the matrix that is the inverse of y)
        
}