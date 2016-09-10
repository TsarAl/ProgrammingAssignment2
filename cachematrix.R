# Creates a Matrix with the Inverse being able to be
# set and cached
makeCacheMatrix <- function(mX = matrix()) 
{
        invX <- NULL
        set  <- function(y) 
		{
			mX <<- y
	                invX <<- NULL
		}

        get  <- function() 
		{
			mX
		}

        setInverse <- function(inverse) 
		      {
			invX <<- inverse
		      }

        getInverse <- function() 
		      {
			invX
		      }

        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


# Solve for the inverse of a matrix and set the inverse to
# be cached
cacheSolve <- function(mX, ..., verbose=TRUE) 
{
        ## Return a matrix that is the inverse of 'mX'
	if(class(mX)=='list') 
	{
	        invX <- mX$getInverse()
	        if (!is.null(invX)) 
		{
	                if (verbose==TRUE) message("Getting cached data")
		} else
		{
			mY <- mX$get()
		        invX <- solve(mY, ...)
		        mX$setInverse(invX)
		}
	} else # if class other than list is passed then just solve with no cache
	{
		if (verbose==TRUE) message(paste("Solving for class",class(mX)))
		invX <- solve(mX, ...)
	}
        
	if (verbose==TRUE) return(invX)
}

