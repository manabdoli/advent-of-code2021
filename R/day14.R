#day14
source('R/util.R')

x <- readfilelines('data/day14/input')
x
day14setup <- function(x){
  startstr <- x[1]
  rules <- do.call(rbind, sapply(x[-(1:2)], strsplit, split = ' -> '))
  list(startstr=startstr, rules = rules)
}
params <- day14setup(x)

# one step insertion
oneinsert <- function(xstr, rules){
  matchpos <- lapply(1:dim(rules)[1], function(i){
    tmpstr <- xstr
    outdf <- data.frame()
    pos <- gregexpr(rules[i, 1], text = tmpstr, fixed = TRUE)[[1]]
    while(any(pos!=-1)){
      outdf <- rbind(outdf,
                     data.frame(pos=pos,
                                char=rep(rules[i, 2], length(pos)),
                                rulename=rep(rownames(rules)[i], length(pos))))
      sapply(pos, function(p) substring(tmpstr, p) <<- '-')
      pos <- gregexpr(rules[i, 1], text = tmpstr, fixed = TRUE)[[1]]
    }
    outdf
  })
  matchpos <- do.call(rbind, matchpos)
  # sort positions
  matchpos <- matchpos[order(matchpos$pos), ]
  #print(matchpos)
  inspos <- matchpos
  inspos$pos <- inspos$pos+1
  # split string
  first <- c(1, inspos$pos)
  last <- c(matchpos$pos, nchar(xstr))
  exploded <- substring(xstr, first, last)
  rulesIdx <- sapply(matchpos$rulename, grep, x=rownames(rules))
  paste0(t(cbind(exploded, c(rules[rulesIdx, 2], ''))), collapse = '')
}

curstr <- params$startstr
cat('step ', 0, ': ', curstr, '\n')
for(i in 1:10){
  curstr <- oneinsert(curstr, params$rules)
  if(i<5) cat('step ', i, ': ', curstr, '\n')
}
elems <- strsplit(curstr, split = '')[[1]]
elem.counts <- as.data.frame(table(elems))
print(with(elem.counts, Freq[which.max(Freq)]-Freq[which.min(Freq)]))

# Part II ####
# Using a transition matrix to keep track of counts
params$rules
n <- dim(params$rules)[1]
transMat <- matrix(0, nrow=n, ncol=n)
for(i in 1:n){
  new_states <- with(params, strsplit(rules[i, 1], split = '')[[1]])
  new_states <- with(params, c(paste0(new_states[1], rules[i, 2]),
                               paste0(rules[i, 2], new_states[2])))
  col.idx <- sapply(new_states, grep, params$rules[,1])
  transMat[i, col.idx] <- 1
}
#
# do.call(rbind,
#         as.list(
#           apply(cbind(params$rules, ':', transMat), 1, paste0, collapse = '')))

# Starting states
m <- nchar(params$startstr)
curStates <- substring(params$startstr, 1:(m-1), 2:m)
curStateCount <- matrix(0, nrow = n, ncol = 1)
sapply(curStates, function(state) {
  s.idx <- which(params$rules[,1]==state)
  if(length(s.idx)>0)
    curStateCount[s.idx] <<- curStateCount[s.idx]+1
})

# do.call(rbind,
#         as.list(
#           apply(cbind(params$rules, ':', curStateCount), 1, paste0, collapse = '')))

#
trans.prod <- diag(n)
# Adding new states
for(k in 1:40){
  trans.prod <- trans.prod %*% transMat
}
# cbind(params$rules, t(t(curStateCount)%*%trans.prod))
sprintf('%25f', sum(t(curStateCount)%*%trans.prod))

#
elemCounts <-
  data.frame(
    elements =
      do.call(rbind,
              sapply(params$rules[,1], function(r) strsplit(r, split = ''))),
    freq = t(t(curStateCount)%*%trans.prod))

#
elems <- unique(c(elemCounts[,1], elemCounts[,1]))
counts <- as.list(rep(0, length(elems)))
names(counts) <- elems
counts[[substr(params$startstr, 1, 1)]] <- counts[[substr(params$startstr, 1, 1)]]+1
counts[[substr(params$startstr, m, m)]] <- counts[[substr(params$startstr, m, m)]]+1
sapply(1:n, function(j){
  counts[[elemCounts[j, 1]]] <<- counts[[elemCounts[j, 1]]] + elemCounts$freq[j]
  counts[[elemCounts[j, 2]]] <<- counts[[elemCounts[j, 2]]] + elemCounts$freq[j]
})
counts <- sapply(counts, function(z) z/2)
sapply(counts, sprintf, fmt='%25.0f')
sprintf('%40f', max(unlist(counts)) - min(unlist(counts)))
