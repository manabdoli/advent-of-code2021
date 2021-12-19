# utility functions

readtxtlines <- function(txt){
  readLines(con = textConnection(txt))
}
readfilelines <- function(filename){
  txtFile <- file(filename)
  x <- readLines(con = txtFile)
  close(txtFile)
  x
}


txt2digits <- function(x){
  xmat <- do.call(
    rbind,
    sapply(x,
           function(r) as.integer(strsplit(r, split = '')[[1]]),
           simplify = FALSE)
  )
  colnames(xmat) <- NULL
  rownames(xmat) <- NULL
  xmat
}

# Matrix Index / Array Position conversions
# apos2midx: array position (found by which) to matrix index
apos2midx <- function(pos, nrow){
  cols <- ((pos-1) %/% nrow) + 1
  rows <- (pos-1) %% nrow + 1
  cbind(rows, cols)
}

# matrix index to array position
midx2aidx <- function(idx, nrow){
  (idx[,2]-1)*nrow + idx[,1]
}

# Find adjacent cells
# adjacent matrix cell indices
adjmatidx <- function(i, j, nrow, ncol, diag=TRUE){
  idx <- do.call(
    rbind,
    sapply(
      i+(-1:1),
      function(h){
        do.call(
          rbind,
          sapply(j+(-1:1), function(k){cbind(h, k)}, simplify = FALSE)
        )
      }, simplify = FALSE)
  )
  # remove center
  ridx <- 5
  # clip outside indexes
  ridx <- c(5, which(idx[,1]<1 | idx[,1]>nrow |
                       idx[,2]<1 | idx[,2]>ncol))
  # remove diagonal ones if needed
  if(!diag){
    ridx <- union(ridx, c(1, 3, 7, 9))
  }
  idx <- idx[-ridx, ]
  idx
}

#adjmatidx(3,3, 5, 5)
#adjmatidx(3,3, 5, 5, F)


## stack:
# Creating a stack with push and pop functions
stackobj <- function(x=NULL, ...){
  UseMethod('stackobj')
}

stackobj.default <- function(x=NULL){
  #
  len <- length(x)
  # push
  push <- function(element){
    x[[len+1]] <<- element
    len <<- len + 1
    x
  }
  # pop
  pop <- function(){
    element <- x[[len]]
    len <<- len - 1
    element
  }
  # Reset
  reset <- function(){
    x<<-NULL
    len<<-0
    x}
  # clean
  clean <- function(){
    if(length(x)>len)
      x <<- x[1:len]
    x
  }
  structure(
    list(
      push=push,
      pop=pop,
      len=function() len,
      top=function() x[[len]],
      clear=reset,
      reset = reset,
      clean = clean,
      stack=function() x
    ),
    class = 'stack'
  )
}

stackobj.matrix <- function(x=NULL){
  #
  len <- NROW(x)
  # push
  push <- function(element){
    x <<- rbind(element, x)
    len <<- NROW(x)
    x
  }
  # pop
  pop <- function(k=1){
    element <- x[1:k, , drop = FALSE]
    len <<- len - k
    clean()
    element
  }
  # Reset
  reset <- function(){
    x<<-NULL
    len<<-0
    x}
  # clean
  clean <- function(){
    if(NROW(x)>len)
      x <<- x[1:len, , drop = FALSE]
    x
  }
  structure(
    list(
      push=push,
      pop=pop,
      len=function() len,
      top=function() x[1, ],
      clear=reset,
      reset = reset,
      clean = clean,
      stack=function() x[1:len,]
    ),
    class = 'stack'
  )
}

print.stack <- function(x, ...){
  cat('Stack of ', x$len(), ' objects: ')
  if(x$len()>0){
    if(!is.list(x$top()))
      cat(paste0(x$stack(), collapse = ', '))
    else
      sapply(x$stack(), print)
  }
  cat('\n')
}
