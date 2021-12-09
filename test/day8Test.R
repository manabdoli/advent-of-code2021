# day8 test
# x <- 'acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
# cdfeb fcadb cdfeb cdbaf'

x <- readLines(con = textConnection(
'be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce'
               )
)

xy <- strsplit(x, split=' | ', fixed=TRUE)
puzzle <- lapply(xy, function(aCase){
  list(
    learn = strsplit(aCase[1], split = ' ')[[1]],
    solve = strsplit(aCase[2], split = ' ')[[1]]
  )
}
)

# 7-segment characteristics
segs <- function(digit, invert=FALSE){
  if(!invert){
    switch(digit+1,
           'abcefg', 'cf', 'acdeg', 'acdfg',
           'bcdf', 'abdfg', 'abdefg', 'acf',
           'abcdefg', 'abcdfg'
    )
  } else{
    switch(digit+1,
           'd', 'abdeg', 'bf', 'be',
           'aeg', 'ce', 'c', 'bdeg',
           '', 'e'
    )
  }
}

# Just to test if I got them right
print7seg <- function(digit){
  segstr <- ' aaaa
b    c
b    c
 dddd
e    f
e    f
 gggg '
  rmvSegs <- strsplit(segs(digit, invert = TRUE), split = '')[[1]]
  for(ch in rmvSegs)
    segstr <- gsub(pattern = ch, replacement = '', segstr)
  cat(segstr, '\n')
}

#print7seg(9)
digterms=sapply(0:9, segs)
seg2dig <- sapply(0:9, list)
names(seg2dig) <- digterms

segDF <- data.frame(digits=0:9, segs=sapply(0:9, segs),
                    len=nchar(sapply(0:9, segs)))
segDF$is.uniq <- sapply(0:9,
                        function(k) sum(segDF$len[k+1]==segDF$len)==1)
uniq.lens <- segDF$len[segDF$is.uniq]
uniq.count <- sapply(puzzle, function(l){
  sum(sapply(l$solve, function(ges) nchar(ges)%in%uniq.lens))
})
sum(uniq.count)


segEnd <- letters[1:7] # position of segments for a to g
sigEnd <- rep(NA, 7)  # position of wires for a to g (not known yet)

# Reading through the 10 signals, transition between signals and segments
# should be built by eliminating non-possible transitions
transMat <- matrix(1, nrow=10, ncol=10) # all transitions are possible

# Part 1:
# the number of characters are used to eliminate the impossible transitions



# Digit-Segment Matrix
dig.seg.mat <- matrix(0, nrow = 10, ncol = 7,
                      dimnames = list(0:9, letters[1:7]))
sapply(letters[1:7],
       function(s) dig.seg.mat[grep(s, sapply(0:9, segs)), s]<<- 1)

colSums(dig.seg.mat)
rowSums(dig.seg.mat)


# Goal to find the transition from a scarmbled signal to segments



#
#

sig.seg.mat <- matrix(1, nrow=7, ncol=7,
                      dimnames = list(LETTERS[1:7], letters[1:7]))
sig.seg.mat
## b
segIdx <- which(letters[0:7]=='b')
sigIdx <- which(csums==6)
sig.seg.mat[sigIdx, -segIdx] <- 0
sig.seg.mat[-sigIdx, segIdx] <- 0

## e
segIdx <- which(letters[0:7]=='e')
sigIdx <- which(csums==4)
sig.seg.mat[sigIdx, -segIdx] <- 0
sig.seg.mat[-sigIdx, segIdx] <- 0

## f
segIdx <- which(letters[0:7]=='f')
sigIdx <- which(csums==9)
sig.seg.mat[sigIdx, -segIdx] <- 0
sig.seg.mat[-sigIdx, segIdx] <- 0

## 1
y[which(rsums==2)]
segs(1)
sig.seg.mat


# Upper to lower relation
u2l <- function(upper, lower){
  sig.seg.mat <- matrix(1, nrow=7, ncol=7,
                        dimnames = list(LETTERS[1:7], letters[1:7]))
  # upper letters
  u <- strsplit(upper, split = '')[[1]]
  uidx <- sapply(u, function(s) which(LETTERS[1:7]==s))
  # lower letters
  l <- strsplit(lower, split = '')[[1]]
  lidx <- sapply(l, function(s) which(letters[1:7]==s))
  #
  sig.seg.mat[-uidx, lidx] <- 0
  sig.seg.mat[uidx, -lidx] <- 0
  sig.seg.mat
}

term2idx <- function(s){
  chars <- strsplit(tolower(s), split = '')[[1]]
  sapply(chars, function(chr) which(letters[1:7]==chr))
}
#term2idx('cdf')
idx2term <- function(idx, upper=FALSE){
  paste0(if(upper) LETTERS[idx] else letters[idx], collapse = '')
}
#idx2term(c(3,4,1))
idx2arr1 <- function(idx, n=7){
  a <- rep(0, n)
  a[idx] <- 1
  a
}
#idx2arr1(c(1,3,7))

U=toupper(puzzle[[1]]$learn)
# Cracking the transition from signal to segment
crack <- function(signals){
  # extract counts used in rules
  U <- toupper(signals)
  # Calculating length for length-based rules
  U.len <- sapply(U, nchar)
  # Calculating number of letters in digits and length of terms
  # in 10 signal terms
  ## The digit-to-signal transition matrix
  dig.sig.mat <- matrix(0, nrow = 10, ncol = 7,
                        dimnames = list(0:9, LETTERS[1:7]))
  sapply(LETTERS[1:7],
         function(s) dig.sig.mat[grep(s, U), s]<<- 1)
  # number of letters used in terms
  csums <- colSums(dig.sig.mat)
  # length of terms for each digit
  rsums <- rowSums(dig.sig.mat)

  # Signal to Segment transition matrix
  # (initially any transition is possible)
  sig.seg.mat <- matrix(1, nrow=7, ncol=7,
                        dimnames = list(LETTERS[1:7], letters[1:7]))

  # signal terms
  sig.terms <- strsplit(U, split = ' ')

  # Employing rules to eliminate transitions -------------
  testAllRules = FALSE # Use TRUE to eliminate some of the tests
  # Testing if the solution if found, when needed
  solFound <- function()
    all(colSums(sig.seg.mat)==1 & rowSums(sig.seg.mat)==1)

  ## From column sums of the transition matrix
  ### b is the only one that is used 6 times
  bEq <- LETTERS[which(csums==6)]
  sig.seg.mat <- sig.seg.mat * u2l(bEq, 'b')
  ### e is the only one that is used 4 times
  eEq <- LETTERS[which(csums==4)]
  sig.seg.mat <- sig.seg.mat * u2l(eEq, 'e')
  ### f is the only one that is used 9 times
  fEq <- LETTERS[which(csums==9)]
  sig.seg.mat <- sig.seg.mat * u2l(fEq, 'f')
  ### a and c are used 8 times.
  acEq <- idx2term(which(csums==8), upper = T)
  sig.seg.mat <- sig.seg.mat * u2l(acEq, 'ac')
  ### d and g are used 7 times.
  dgEq <- idx2term(which(csums==7), upper = T)
  sig.seg.mat <- sig.seg.mat * u2l(dgEq, 'dg')


  ## From row sums of the transition matrix
  ### 1 is the only digit with length of 2
  d1Eq <- U[which(U.len==2)]
  sig.seg.mat <- sig.seg.mat * u2l(d1Eq, segs(1))
  ### 4 is the only digit with length of 4
  d4Eq <- U[which(U.len==4)]
  sig.seg.mat <- sig.seg.mat * u2l(d4Eq, segs(4))
  # At this stage the a unique transition would've been found

  if(testAllRules | !solFound()){
    ### 7 is the only digit with length of 3
    d7Eq <- U[which(U.len==3)]
    sig.seg.mat <- sig.seg.mat * u2l(d7Eq, segs(7))
  }

  if(testAllRules | !solFound()){
    ### 8 is the only digit with length of 7
    d8Eq <- U[which(U.len==7)]
    sig.seg.mat <- sig.seg.mat * u2l(d8Eq, segs(8))
  }

  if(testAllRules | !solFound()){
    ### 0, 6 and 9 digits have 6 letters
    d069Eq <- U[which(U.len==6)]
    for(s in d069Eq){
      sig.seg.mat <- sig.seg.mat *
        (u2l(s, segs(0)) | u2l(s, segs(6)) | u2l(s, segs(9)))*1.0
    }
  }

  if(testAllRules | !solFound()){
    ### 2, 3 and 5 digits have 5 letters
    d235Eq <- U[which(U.len==5)]
    for(s in d235Eq){
      sig.seg.mat <- sig.seg.mat *
        (u2l(s, segs(2)) | u2l(s, segs(3)) | u2l(s, segs(5)))*1.0
    }
  }
  # return the transition
  sig.seg.mat
}

signals2digits <- function(signals){
  # Find signals to segment transition
  sig.seg.mat <- crack(signals)
  # Find signals to segments
  sig2seg <-
    sapply(toupper(signals), # do this for all terms
           function(sig){
             paste0( # join the segments
               sapply(term2idx(sig), # for all signals
                    function(j){ # find the transition to segment
                      letters[which(sig.seg.mat[j,]==1)]
                    }),
               collapse = '')
           })
  # find signal to digits using segments
  sig2dig <-sapply(sig2seg, function(seg) seg2dig[seg])
}

