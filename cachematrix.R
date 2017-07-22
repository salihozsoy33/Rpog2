## This program contains 2  functions that cache the
## inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## However, it first checks to see if the nverse has already been calculated
## If so cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting data from the cache ")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m     
}
