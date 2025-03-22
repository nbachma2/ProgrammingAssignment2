
## makeCacheMatrix generates variables and a list of functions that allows to either set or 
#retrieve values to/from a cache matrix

makeCacheMatrix <- function(x = matrix()) {
        #set up inverse matrix variable
        minv<-NULL

        #define each of the required functions
        
        #setmatrix will take a matrix as input and store it to a variable called m_cache in the parent environment; 
        #minv_cache is set to NULL to indicate that the matrix has changed and a previously cached matrix is outdated
        setmatrix<-function(y = matrix()){
                x<<-y
                minv<<-NULL
        }
        
        #define getmatrix as function that simply retrieve the value of the cached matrix x
        getmatrix<-function() x
        
        #setinverse will store the input (inverse matrix) in parent environment
        setinverse<-function(inverse) minv<<-inverse
        
        #getinverse will retrieve the inverse matrix from the parent environment
        getinverse<-function() minv
        #return a list of these four functions to be used in cacheSolve
        list(setmatrix=setmatrix, getmatrix=getmatrix,
             setinverse=setinverse,
             getinverse=getinverse)
        
}

## cacheSolve will take the list of functions defined above as input and refer to the cache data to either 
## calculate and store the inverse matrix, or recall the previously calculated inverse matrix

cacheSolve <- function(x, ...) {
        #retrieve cached inverse matrix
        minv<-x$getinverse()
        #if inverse matrix is already stored, retrieve that instead of calculating again
        if(!is.null(minv)){
                message("getting cached inverse matrix")
                return(minv)
        }
        #otherwise, retrieve matrix, calculate inverse matrix, store it and return inverse
        else{
                data<-x$getmatrix()
                minv<-solve(data,...)
                x$setinverse(minv)
                minv
        }
}
