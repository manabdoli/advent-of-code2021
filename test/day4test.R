# day 4
## Test ####

draw <- c(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1)

boards <- read.table(
  text = '
22 13 17 11  0
8  2 23  4 24
21  9 14 16  7
6 10  3 18  5
1 12 20 15 19

3 15  0  2 22
9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
2  0 12  3  7
'
)

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
  for(i in players){
    if(any(colSums(trklst[[i]])==5) | any(rowSums(trklst[[i]])==5))
      return(i)
  }
  return(0)
}


whowon <- 0
for(j in draw){
  updtrk(j)
  whowon <- winner()
  if(whowon>0){
    #browser()
    players <- players[-which(players==whowon)]
    winners <- c(winners, whowon)
    if(length(players)==0){
      break
    }
  }
}
winners
brdlst[[whowon]]
trklst[[whowon]]

if(whowon>0){
  j*sum((1-trklst[[whowon]])*brdlst[[whowon]])
}
