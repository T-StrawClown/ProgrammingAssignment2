# Solution for programming assignment. Functions below are derived 
# from example in the assignment. 
# First function makeCacheMatrix creates special list object, wich is 
# able to store matrix as well as inverse matrix.
# The second function cacheSolve determines if it needs to calculate inverse
# matrix for given matrix, or it can get the result from previously
# calculated and stored result. In addition to example it can also identify
# if 2 consequent dynamic calls have the same underliying matrix and use
# cached result in that case too (see function description)

## Returns list, that holds matrix and inverse matrix. Has the following functions:
# - set() saves original matrix with list object
# - get() retrieves original matrix from list object
# - setinv() saves inverse matrix with list object
# - getinv() retrieves inverse matrix from list object
#
# e.g. how to use
# m1 <- matrix(sample(10:15,25,replace=T),5,5) ## 5x5 matrix
# cm1 <- makeCacheMatrix(m1)
# cm1 is a list that, contains original matrix and inverse matrix after 
# cacheSolve(cm1) is called

makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL
        set <- function(y) {
                x <<- y
                invx <<- NULL
        }
        get <- function() {
                return(x)
        }
        setinv <- function(inverce) {
                invx <<- inverce
                return(invx)
        }
        getinv <- function() {
                return(invx)
        }
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Returns inverse matrix of given matrix. Checkes if results can be retrieved
# from cache or have to be calculated for pre-defined list objects (like cm1
# described above). In case of dynamic call (when no pre-defined list object is
# created before call) creates last_dynamic_call variable (list object in calling
# environment). As long as 2 subsequent dynamic calls have identicall matrices it
# will get results from cache as well.
# Will report if results are calculated or retrieved from previously saved. 
#
# example 1 (pre-defined list objec)
# cacheSolve(cm1) # returns result from cache from 2nd call on
# example 2 (dynamic call)
# cacheSolve(makeCacheMatrix(m1))
# returns result from cache from 2nd call on as long as m1 is not changed


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## deal with dynamic call
        if(class(substitute(x)) == "call") {
                ## create variable last_dynamic_call in calling environment if needed
                if(!exists("last_dynamic_call", where = parent.frame())) {
                        message("creating variable last_dynamic_call for dynamic calls")
                        last_dynamic_call <<- x
                }
                
                ## check if underlying matrix has changed
                if(!identical(x$get(), last_dynamic_call$get())) {
                        message("last_dynamic_call has different matrix")
                        last_dynamic_call$set(x$get())
                }
                ## recursivelly call itself
                return(cacheSolve(last_dynamic_call))
        }
        
        ## normal routine, no need to change too much from sample code
        ## except use solve to calculate inverse matrix
        invx <- x$getinv()
        if(!is.null(invx)) {
                message("getting cached data")
                return(invx)
        }
        else {
                message("calculating inverse matrix")
                data <- x$get()
                invx <- solve(data,...)
                x$setinv(invx)
                return(invx)
        }
}
