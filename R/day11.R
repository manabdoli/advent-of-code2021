# day 11 Test

# Read text
readtxt <- function(txt){
  x <- readLines(con = textConnection(txt))
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

readtxtFile <- function(filename){
  txtFile <- file(filename)
  x <- readLines(con = txtFile)
  close(txtFile)
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

# adjacent cells
adjcells <- function(i, j, nm=rc){
  idx <- do.call(
    rbind,
    sapply(
      i+(-1:1),
      function(h){
        do.call(
          rbind,
          sapply(
            j+(-1:1),
            function(k){
              cbind(h, k)
            }, simplify = FALSE)
        )
      }, simplify = FALSE)
  )
  # remove center
  ridx <- 5
  # clip outside indexes
  ridx <- c(5, which(idx[,1]<1 | idx[,1]>nm[1] |
                  idx[,2]<1 | idx[,2]>nm[2]))
  #if(length(ridx)>0) no need to test since at least ridx=5
    idx <- idx[-ridx, ]
  idx
}
#adjcells(1, 1)

# pos2idx: position (found by which) to index
pos2idx <- function(pos, nm=rc){
  cols <- ((pos-1) %/% nm[1]) + 1
  rows <- (pos-1) %% nm[1] + 1
  cbind(rows, cols)
}

# Part I ####
xmat <- readtxtFile('data/day11/input')
rc <- dim(xmat)


steps <- 100
flashes <- 0
firstfullflash <- ''
for(i in 1:steps){
  # increase
  xmat <- xmat + 1
  # Find flashes
  fpos <- which(xmat==10)
  # Propagate flashes
  while(length(fpos)>0){
    # cur flash
    curidx <- pos2idx(fpos[1])
    # Adjacent cells
    adjidx <- adjcells(curidx[1], curidx[2])
    # increase adjacents
    xmat[rbind(curidx, adjidx)] <- xmat[rbind(curidx, adjidx)]+1
    # any new flash?
    newfpos <- setdiff(which(xmat==10), fpos)
    if(length(newfpos)>0){
      fpos <- c(fpos, newfpos)
      flashes <- flashes + length(newfpos)
    }
    # remove processed cell
    fpos <- fpos[-1]
  }
  # flash - reset to zero
  zidx <- pos2idx(which(xmat>10))
  xmat[zidx] <- 0
  # check:
  # print(xmat)
}
print(flashes)

# Part II ####
xmat <- readtxtFile('data/day11/input')
rc <- dim(xmat)
steps <- 0
flashes <- 0
firstfullflash <- ''
while(TRUE){
  steps <- steps+1
  # increase
  xmat <- xmat + 1
  # Find flashes
  fpos <- which(xmat==10)
  flashes <- flashes + length(fpos)
  # Propagate flashes
  while(length(fpos)>0){
    # cur flash
    curidx <- pos2idx(fpos[1])
    # Adjacent cells
    adjidx <- adjcells(curidx[1], curidx[2])
    # increase adjacents
    xmat[rbind(curidx, adjidx)] <- xmat[rbind(curidx, adjidx)]+1
    # any new flash?
    newfpos <- setdiff(which(xmat==10), fpos)
    if(length(newfpos)>0){
      fpos <- c(fpos, newfpos)
      flashes <- flashes + length(newfpos)
    }
    # remove processed cell
    fpos <- fpos[-1]
  }
  # flash - reset to zero
  zidx <- pos2idx(which(xmat>10))
  xmat[zidx] <- 0
  # check:
  if(all(xmat==0)){
    firstfullflash <-
    c(firstfullflash, sprintf('%g flash in step %g',
                              length(firstfullflash), steps))
    break
  }
}
print(flashes)
print(firstfullflash)
