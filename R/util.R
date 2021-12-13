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
