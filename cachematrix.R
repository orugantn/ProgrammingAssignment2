##Objective of this function is to estimate inverse of a matrix and save in cache to retrive as needed

##Large data computation is expensive - time consuming and memory usage - and it can be handled with saving the computation result in the cache to retrive as needed. 
makeCacheMatrix <- function(specialMatrix = matrix()){
       
       #Initialize inverse matrix to null
       invMat <- NULL
       
       #Function to set new Matrix
       setMat <- function(newMatrix){
              specialMatrix <<- newMatrix
              
              #Reinitalize inverse matrix to null for new matrix
              #Erase inverse matrix in cache
              invMat <<- NULL
       }
       
       #Function to get Matrix
       getMat <- function(){specialMatrix}
       
       #Function to set inverse matrix
       setInvMat <- function(setInvMat){
              #Replace cached inverse matrix with setInvMat
              invMat <<- setInvMat
       }
       
       #Function to get Inverse Matrix
       getInvMat <- function(){
              invMat
       }
       
       #List all setMat, getMat, setInvMat and getInvMat functions
       list(setMat = setMat, getMat = getMat,
            setInvMat = setInvMat,
            getInvMat = getInvMat)
}


##Objective of this function is to estimate inverse of a matrix and return to makeCacheMatrix function when requested.

##Argument for this function is a makeCacheMatrix Function, along with arguments required for solve function
cacheSolve <- function(cacheMatrixFun, ...){
       
       #Local initialization of inverse matix
       #It is the result of getInvMat function of makeCacheMatrix Function
       invMat <- cacheMatrixFun$getInvMat()
       
       #If inverse matrix is not null then return cached inverse matrix
       if(!is.null(invMat)){
              message("getting cached inverse of a matrix")
              return(invMat)
       }
       
       #Estimate inverse of a matrix and pass to the makeCacheMatrix function
       #specialMatrix is local variable
       specialMatrix <- cacheMatrixFun$getMat()
       invMat <- solve(specialMatrix, ...)
       cacheMatrixFun$setInvMat(invMat)
       
       #Print ouput of inverse matrix
       invMat
}