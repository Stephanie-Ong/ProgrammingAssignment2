## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function consists of 7 functions:-
		## set() 			 -resets cache value by entering: makeCacheMatrix()$set()
		## get() 			 -displays the contents of 'x'
		## setSqMatrix() 	 -caches input into 'sm'
		## getSqMatrix() 	 -returns the contents of 'sm' if 'sm' exists
		## setInverse()		 -caches input into 's'
		## getInverse() 	 -returns the contents of 's' if 's' exists
		## compareSqMatrix() -compares newly input Square Matrix with cached Square Matrix. Returns 'TRUE' when it matches and 'FALSE' when it does not match
		
## This function serves as an input to cacheSolve(). eg cacheSolve(makeCacheMatrix(matrix(1:4,nrow=2,ncol=2)))
## If ran on its own, makeCacheMatrix() will return a list of all the 6 functions in the abovementioned.

makeCacheMatrix <- function(x = matrix()) {
  ##function 0: set() resets cache value by with makeCacheMatrix()$set()
  set <- function(y= matrix()) {
    x <<- y
    s <<- NULL
    sm <<-NULL
  }
  
  ##function 1: get() displays the contents of 'x'
  get <- function() 
  {
    x
  }
  
  ##function 2: setSqMatrix() caches input into 'sm'
  setSqMatrix <- function(SqMatrix) 
  {
    sm <<- SqMatrix
  }
  
  ##function 3: getSqMatrix() returns the contents of 'sm' if 'sm' exists
  getSqMatrix <- function() 
  {
    if (exists('sm')) 
    {
      sm
    } 
    else 
    {
      return(NULL)
    }
  }
  
  
  ##function 4: setInverse caches input into 's'
  setInverse <- function(solve) 
  {
    s <<- solve
  }
  
  ##function 5: getInverse() returns the contents of 's' if 's' exists
  getInverse <- function() 
  {
    if (exists('s')) 
    {
      s
    } 
    else 
    {
      return(NULL)
    }
  }
  ##function 6: compareSqMatrix() compares newly input Square Matrix with cached Square Matrix. Returns 'TRUE' when it matches and 'FALSE' when it does not match
  compareSqMatrix <- function(newSqMatrix,cachedSqMatrix)
  {
    if((nrow(cachedSqMatrix)==nrow(newSqMatrix))&&(ncol(cachedSqMatrix)==ncol(newSqMatrix))) ## compares the dimensions of the input and cache matrix. Run if match
    {
      if(is.na(table(sapply(1:ncol(newSqMatrix),function(n){newSqMatrix[n,]==cachedSqMatrix[n,]}))["TRUE"]))
      { 
        ## returns false when new Square Matrix does not match cached Square Matrix
        FALSE
      }
      else if(table(sapply(1:ncol(newSqMatrix),function(n){newSqMatrix[n,]==cachedSqMatrix[n,]}))["TRUE"] ==length(newSqMatrix)) ## compares the elements in the input and cache matrix. Run if match
      {	
        ## returns true when new Square Matrix matches cached Square Matrix
        TRUE
      }
      else 
      {
        ## returns false when new Square Matrix does not match cached Square Matrix
        FALSE
      }
    }
    else
    {
      ## returns false when new Square Matrix does not matchcached Square Matrix
      FALSE
    }
    
  }
  
  # displays all the functions 0 to 6 in a list
  list(set = set,
       get = get,
       setSqMatrix = setSqMatrix,
       getSqMatrix = getSqMatrix,
       compareSqMatrix = compareSqMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}





## Write a short comment describing this function
## This function must have the makeCacheMatrix() function with a square matrix as an input. eg cacheSolve(makeCacheMatrix(matrix(1:4,nrow=2,ncol=2)))
## It calls on the various functions that are available in cacheSolve() to do the following:
		##1- retrieves the cached inverse matrix
		##2- retrieves the new square matrix
		##3- if 'cached inverse matrix' exists, it compares the 'new square matrix' against the 'cached square matrix'
		##4- if 'cached inverse matrix' does not exist, it sets 'getCache' flag to FALSE
		##5- if the 'new square matrix' matches the 'cached square matrix', it sets 'getCache' flag to TRUE
		##6- Returns a list that displays the process type & the cached values depending on the 'getCache' flag
		##7- The 2 possible process types are: "getting cached data ... " and "calculating inverse matrix ... "
## This function will only work with square matrices
## There is no exception handling for the input of classes other than those specified.
		


cacheSolve <- function(x, ...) 
{
  s1 <- x$getInverse()  
  data <- x$get() ## passes the input attribute into 'data'
  if(!is.null(s1)) ## checks for cached inversed matrix. run if it exists
  {
    SqMatrix <- x$getSqMatrix() ## retrieves cached matrix of the inverse into 'SqMatrix'
    if(x$compareSqMatrix(data,SqMatrix)=='TRUE')
    {
      getCache <- TRUE ## getCache flag set to TRUE
    }
    else
    {
      getCache <- FALSE ## getCache flag set to FALSE
    }  
    
  }
  else
  {
    getCache <- FALSE ## getCache flag set to FALSE
  } 
  if (getCache == TRUE)
  {
    list(Process=paste("getting cached data ... "),Square.Matrix=sm,Inverse.Matrix=s)
  }
  else
  {
    x$setInverse(solve(data, ...)) ## cache new inverse matrix into 's'
    x$setSqMatrix(data) ## cache new matrix into 'sm'
    list(paste("calculating inverse matrix ... "),Square.Matrix=sm,Inverse.Matrix=s)
  }
  
} 
