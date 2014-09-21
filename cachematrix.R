## These 2 functions cache the inverse of a matrix so that you do not need to
## repeatedly compute it

## This function returns a list of functions that set and get the value and 
##inverse of a matrix


makeCacheMatrix <- function(x = numeric()){
        minv<- NULL
        set <- function(y) {
                x <<- y
                minv<<- NULL
        }
        get <- function () x
        setinv <-function(solve) minv<<-solve
        getinv <-function() minv
        list(set = set, get = get,setinv = setinv,getinv = getinv)
}

## This function will retrieve the inverse of a matrix if it has already been 
##calculated or compute it if it has not been

cacheSolve <- function(x,...){
        minv <- x$getinv()
        if(!is.null(minv)){
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv<-solve(data, ...)
        x$setinv(minv)
        minv
}