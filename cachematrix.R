##the function returns a list containing the matrix and its inverse and includes functions to set the matrix and its inverse's value in the attribute variables in the parent environment
makeCacheMatrix <- function(x = matrix()) {
   flag<-NULL
   inverse<-NULL
   makemat<-function(y){
       x<<-y
       
   }
   setinv<-function(y)
   {
       inverse<<-solve(x)
       flag<<-1
   }
   getmat<-function(){x}
   getflag<-function(){flag}
   getinv<-function(){inverse}
   list(makemat=makemat,getmat=getmat,getflag=getflag,setinv=setinv,getinv=getinv)
}



##this function returns the inverse of the special matrix x, if already calculated its just obtains it from the cache and returns it else it is calculated 
cacheSolve <- function(x, ...) {
       m<-x$getinv()
       if(!is.null(m))
            return(m)
       else{
            k<-solve(x$getmat())
            x$setinv(k)
            return(x$getinv())
        }
 }
