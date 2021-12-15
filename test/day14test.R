#day14test
source('R/util.R')

x <- readtxtlines('NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C')
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
  print(matchpos)
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
with(elem.counts, Freq[which.max(Freq)]-Freq[which.min(Freq)])
## Template:     NNCB
## After step 1: NCNBCHB
#                NCNBCHB
## After step 2: NBCCNBBBCBHCB
#                NBCCNBBBCBHCB
## After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
#                NBBBCNCCNBBNBNBBCHBHHBCHB
## After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB
#                NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB
