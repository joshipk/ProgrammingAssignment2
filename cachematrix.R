## Following functions use the objects already available in cache to save the 
## processing time. makeCacheMatrix function takes a matrix input and outputs 
## list of functions to enable the cache handling for this matrix. cacheSolve 
## function checks whether matrix inverse exists or not and based on that takes the action.

## makeCacheMatrix takes  a square invertible matrix as input and 
## returns a list containing functions to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse
## 4. get the inverse
## This list is used as the input to cacheSolve().
        

makeCacheMatrix <- function(x = matrix()) {

        inv = NULL                                                                   
        set = function(y) {                                                          
                # use `<<-` to assign a value to an object in an environment         
                # different from the current environment.                            
                x <<- y                                                              
                inv <<- NULL                                                         
        }                                                                            
        get = function() x                                                           
        setinv = function(inverse) inv <<- inverse                                   
        getinv = function() inv                                                      
        list(set=set, get=get, setinv=setinv, getinv=getinv)                         
} 


## cacheSolve takes the matrix used in makeCacheMatrix function as input
## as well as the list of functions created.
## It checkes whether inverse of this matrix exists or not. If yes,
## gets the inverse from cache else inverts it.
## Returns the inverse of inpute matrix x.


cacheSolve <- function(x, ...) {                                                     
        ## x is output of makeCacheMatrix()                                           
        ## return inverse of the matrix x         
                                                                                     
        inv = x$getinv()                                                             
                                                                                     
        # if the inverse exists in cache then                               
        if (!is.null(inv)){                                                          
                # get it from the cache and returns the value.                      
                message("getting cached data")                                      
                return(inv)                                                          
        }                                                                            
                                                                                     
        # otherwise, calculates the inverse matrix
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # set the value of the inverse to the cache via setinv function.
        x$setinv(inv)
        
        return(inv)
}                                                                                   
                                                                                     
