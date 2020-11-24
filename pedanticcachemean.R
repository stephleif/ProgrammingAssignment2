## from the discussion board, this version of cachemean helps to
## show what variables are passed in

cachemean <- function(makeVector.object, ...) {
    m.local <- makeVector.object$getmean()
    if(!is.null(m.local)) {
        message("getting cached data")
        return(m.local)
    }
    data <- makeVector.object$get()
    m.local.calculated <- mean(data, ...)
    makeVector.object$setmean(m.local.calculated)
    m.local.calculated # return the mean value
}
