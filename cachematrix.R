## This function will insert any value of
## the variable x as the matrix function will set as cachematrix
## conditional: if not, x =  NULL 

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }

  get<-function() x
  setInv<-function(inverse) inv<<-inverse
  getInv<-function() inv
  list(set=set,
       get=get,
       setInv=setInv,
       getInv=getInv)
}

## This line will set you an inversed matrix whereas
## caches it so you can retrieve. the cache will retrieve and send forward later on the function
## named as cacheSolve


cacheSolve <- function(x, ...) {
  inv<-x$getInv()
  if(!is.null(inv)) {
    message("retrieving matrix")
    return(inv)
  }

  ans<-x$get()
  inv<-solve(ans,...)
  x$setInv(inv)
  inv

}