## Calculating the inverse matrix of a given matrix is a computationally ## intensive process. That's why, we want to cache the value of an inverse matrix ## that we have already calculated and use it again if we need it, rather than ## calculate it over and over again. The functions below allow us to do that: ## calculate the inverse of a matrix and cache it and then check in the cache for ## it and retrieve it withour calculating it for a second time.

## This function can create a special "matrix" object that can cache its inverse
## It is a list of functions that:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse matrix
## 4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	inverse<-NULL
        set<-function(y){		
                x<<-y
                inverse<<-NULL
        }
        get<-function()x		
        setmatrix<-function(solve) inverse<<-solve	
        getmatrix<-function()inverse
        list(set=set,get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## The following function calculates the inverse of the special "matrix" that was ## created in the makeCacheMatrix function. It first checks if it was calculated ## before, so that it doesn't need to make the computation again. If yes, it gets ## it from the cache displaying the message "getting cached data", otherwise it ## calculates it.

cacheSolve <- function(x, ...) {
        ## Check if the inverse has already been calculated. If so, print 
	## "getting cached data" and return the inverse matrix
	inverse<-x$getmatrix()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
	## Return a matrix that is the inverse of 'x'
        data<-x$get()
        inverse<-solve(data,...)
        x$setmatrix(inverse)
        inverse
}
