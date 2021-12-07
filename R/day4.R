# day 4
draw <- read.csv(file = 'input', header = FALSE, as.is = TRUE, colClasses = 'integer', nrows = 1)
draw <- as.numeric(draw)
x <- read.table(file = 'input', skip = 1, colClasses = 'integer')
boards <- as.matrix(x)

range(draw)
range(x)

brdlst <- lapply(1:(dim(boards)[1]/5), function(k) as.matrix(boards[(1:5)+(k-1)*5,]))
trklst <- rep(list(matrix(0, nrow=5, ncol=5)), length(brdlst))

updtrk <- function(k){
  for(i in 1:length(brdlst)){
    idx <- which(brdlst[[i]]==k)
    if(length(idx)>0){
      trklst[[i]][idx] <<- 1
    }
  }
}


winner <- function(){
  for(i in 1:length(brdlst)){
    if(any(colSums(trklst[[i]])==5) | any(rowSums(trklst[[i]])==5))
      return(i)
  }
  return(0)
}

whowon <- 0
for(j in draw){
  updtrk(j)
  whowon <- winner()
  if(whowon>0) break
}
brdlst[[whowon]]
trklst[[whowon]]

if(whowon>0){
  j*sum((1-trklst[[whowon]])*brdlst[[whowon]])
}

## Last Winner ####

players <- 1:length(brdlst)
trklst <- rep(list(matrix(0, nrow=5, ncol=5)), length(brdlst))
winners <- NULL
updtrk <- function(k){
  for(i in players){
    idx <- which(brdlst[[i]]==k)
    if(length(idx)>0){
      trklst[[i]][idx] <<- 1
    }
  }
}


winner <- function(){
  curWinners <- NULL
  for(i in players){
    if(any(colSums(trklst[[i]])==5) | any(rowSums(trklst[[i]])==5))
      curWinners <- c(curWinners, i)
  }
  if(is.null(curWinners)) return(NULL) else curWinners
}


whowon <- 0
for(j in draw){
  updtrk(j)
  whowon <- winner()
  if(!is.null(whowon)){
    #browser()
    for(w in whowon){
      players <- players[-which(players==w)]
      winners <- c(winners, w)
    }
    if(length(players)==0){
      break
    }
  }
}
players
winners
brdlst[[whowon]]
trklst[[whowon]]

if(whowon>0){
  j*sum((1-trklst[[whowon]])*brdlst[[whowon]])
}
