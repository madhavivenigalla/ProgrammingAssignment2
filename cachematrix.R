## The functions makeCacheMatrix , cacheSolve generates the inverse of a matrix and caches

## function makeCacheMatrix is to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL

	set<-function(y){
		x<<-y
		m<<-NULL
	}

	get <-function()x

	setinverse<-function(invm) m<<-invm

	getinverse<-function()m

	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## function cacheSolve check if the inverse of a matrix is cached, if it is then the cached matrix is returned 
## otherwise inverse of a matrix is calculated and will be cached  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m<-x$getinverse()

      if(!is.null(m)){
           message("getting cached data")
           return(m)
      }

      data<-x$get()

      m<-solve(data)

      x$setinverse(m)

      m
}
