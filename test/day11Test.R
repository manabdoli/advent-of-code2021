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

# Test 1
xmat <- readtxt(
'11111
19991
19191
19991
11111'
)
rc <- dim(xmat)


steps <- 2
flashes <- 0
for(i in 1:steps){
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
  print(xmat)
}


# Test 2
xmat <- readtxt(
'5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526'
)
rc <- dim(xmat)


steps <- 100
flashes <- 0
for(i in 1:steps){
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
  print(xmat)
}

ymat <- readtxt(
'0397666866
0749766918
0053976933
0004297822
0004229892
0053222877
0532222966
9322228966
7922286866
6789998766'
)
any(xmat!=ymat)
print(flashes)
