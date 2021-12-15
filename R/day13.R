#day 13
source('R/util.R')
x <- readfilelines('data/day13/input')

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
printsecret <- function(secret, rc=attr(secret, 'rc')){
  if(is.null(rc))
    rc <- c(max(secret[,1]), max(secret[,2]))
  prtstr <- matrix('.', nrow=rc[1]+1, ncol=rc[2]+1)
  prtstr[secret+1] <- '#'
  cat(apply(prtstr, 2, paste0, collapse = ''), sep='\n')
}

# folding
fold <- function(secret, instr, rc=c(max(secret[,1]), max(secret[,2]))){
  if(instr$xy=='x'){
    xidx <- which(secret[,1]>instr$val)
    secret1=NULL
    if(length(xidx)>0){
      secret1 <- cbind(instr$val*2-secret[xidx,1], secret[xidx,2])
      secret <- unique(rbind(secret[-xidx,], secret1))
    }
    rmidx <- which(secret[,1]==instr$val)
    if(length(rmidx)>0)
      secret <- secret[-rmidx, ]
    # shift all to zero
    if(min(secret[,1])<0)
      secret[,1] <- secret[, 1]-min(secret[,1])
    newrc <- c(max(rc[1]-instr$val-1, instr$val-1), rc[2])
  } else{
    yidx <- which(secret[,2]>instr$val)
    secret1=NULL
    if(length(yidx)>0){
      secret1 <- cbind(secret[yidx,1], instr$val*2-secret[yidx,2])
      secret <- unique(rbind(secret[-yidx,], secret1))
    }
    rmidx <- which(secret[,2]==instr$val)
    if(length(rmidx)>0)
      secret <- secret[-rmidx, ]
    # shift all to zero
    if(min(secret[,2])<0)
      secret[,2] <- secret[, 2]-min(secret[,2])
    newrc <- c(rc[1], max(rc[2]-instr$val-1, instr$val-1))
  }
  structure(secret, rc=newrc)
}
#
#
params <- day13setup(x)

# full_rc <- c(params$instr[which(params$instr$xy=='x')[1],2],
#         params$instr[which(params$instr$xy=='y')[1],2])*2+1
#
# cursecret <- structure(params$secret,
#                       rc=full_rc)
cursecret <- params$secret

for(i in 1:dim(params$instr)[1]){
  cursecret <- fold(cursecret, params$instr[i,], attr(cursecret, 'rc'))
  if(i==1)
    cat('Part I (First fold): ', dim(cursecret)[1], '\n')
}
cat('Part II (Final fold): ', dim(cursecret)[1], '\n')
printsecret(cursecret)

