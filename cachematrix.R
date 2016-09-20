## use makeCacheMatrix to construct an object x
## from a square (invertible) matrix and 
## use cacheSolve to get the inverted matrix of x
## from the cache or from the solver if it 
## is not in the cache


## makeCacheMatrix creates an object x
## which can set and get the values contained in a matrix x,
## with x$set() and x$get
## which can also set and get the inverted matrix of x
## with x$getinv() and x$setinv()
makeCacheMatrix <- function(x = matrix()) {
    invx<-NULL
    set<-function(y){
      x<<-y
      invx<<-NULL
    }
    get<-function() x
    setinv<-function(inv) invx<<-inv
    getinv<-function()invx
    list(set=set,get=get,
         setinv=setinv,
         getinv=getinv)
  }

## given an object x created by makeCacheMatrix 
## cacheSolve retrieves the invert of 
## matrix x$get() from cache (if it exists in the cache)
## or calculates the inverse of x$get() otherwise

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invx<-x$getinv()
    if (!is.null(invx)){
      message("getting cached invert matrix")
      return(invx)
    }
    invx<-solve(x$get())
    x$setinv(invx)
    invx
  }

