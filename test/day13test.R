#day13 Test
source('R/util.R')

# Part I
x <- readtxtlines(
  '6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5'
)

day13setup <- function(x){
  sepline <- which(x=='')
  instr <- do.call(
    rbind,
    strsplit(gsub('fold along ', '', x[(sepline+1):length(x)]), split = "="))
  instr <- data.frame(xy=instr[,1], val=as.integer(instr[,2]))
  secret <- do.call(
    rbind,
    lapply(strsplit(x[1:sepline-1], split = ","),
           as.integer))
  list(secret=structure(secret, rc=c(max(secret[,1]), max(secret[,2]))),
       instr=instr)
}

#print sheet
printsecret <- function(secret, rc=c(max(secret[,1]), max(secret[,2]))){
  prtstr <- matrix('.', nrow=rc[1]+1, ncol=rc[2]+1)
  prtstr[secret+1] <- '#'
  cat(apply(prtstr, 2, paste0, collapse = ''), sep='\n')
}

# folding
fold <- function(secret, instr, rc=c(max(secret[,1]), max(secret[,2]))){
  if(instr$xy=='x'){
    xidx <- which(secret[,1]>instr$val)
    secret1 <- cbind(rc[1]-secret[xidx,1], secret[xidx,2])
    secret <- unique(rbind(secret[-xidx,], secret1))
    rmidx <- which(secret[,1]==instr$val)
    if(length(rmidx)>0)
      secret <- secret[-rmidx, ]
    newrc <- c(instr$val-1, rc[2])
  } else{
    yidx <- which(secret[,2]>instr$val)
    secret1 <- cbind(secret[yidx,1], rc[2]-secret[yidx,2])
    secret <- unique(rbind(secret[-yidx,], secret1))
    rmidx <- which(secret[,2]==instr$val)
    if(length(rmidx)>0)
      secret <- secret[-rmidx, ]
    newrc <- c(rc[1], instr$val-1)
  }
  structure(secret, rc=newrc)
}
#
params <- day13setup(x)
printsecret(params$secret)

cursecret <- params$secret
for(i in 1:dim(params$instr)[1]){
  cursecret <- fold(cursecret, params$instr[i,], attr(cursecret, 'rc'))
  printsecret(cursecret, attr(cursecret, 'rc'))
}
