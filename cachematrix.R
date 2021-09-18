## This function will insert any value of the given variable "x" as the matrix function will set as cachematrix. The condition for this would be if not, x is NULL or x = NULL

makeCacheMatrix <- function(x = matrix()) {
  n_INV < - NULL
  set<-function(y) {
    x << - y
     n_INV << - NULL
  }

  get < - function() x
  INV_Set < - function(inverse) n_INV << - inverse
  INV_Get < - function() n_INV
  list(Set = Set,
       Get = Get,
       INV_Set=INV_Set,
        INV_Get= INV_Get)
}

## This line will provide an inverse matrix wherein it caches the function so the used can retrieve it. The said cache will retrieve and send forward later on the function named as cacheSolve


cacheSolve < - function(x, ...) {
  n_INV < - x$ INV_Get()
  if(!is.null( n_INV)) {
   	 message("the code is processing to get the required data")
    	return(n_INV)
  }

  DATA_Ans < - x$get()
  n_INV < - solve(DATA_Ans,...)
  x$INV_Set(n_INV)
  n_INV

}

## End of code.