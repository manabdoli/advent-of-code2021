# day 8
inputFile <- file('data/day08/input')
x <- readLines(con = inputFile)
close(inputFile)

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

digterms=sapply(0:9, segs)
seg2dig <- sapply(0:9, list)
names(seg2dig) <- digterms


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

if(FALSE){
  # Used to figure the rules out
  segDF <- data.frame(digits=0:9, segs=sapply(0:9, segs),
                      len=nchar(sapply(0:9, segs)))
  segDF$is.uniq <- sapply(0:9,
                          function(k) sum(segDF$len[k+1]==segDF$len)==1)
  uniq.lens <- segDF$len[segDF$is.uniq]
  xy <- strsplit(x, split=' | ', fixed=TRUE)
  puzzle <- lapply(xy, function(aCase){
    list(
      learn = strsplit(aCase[1], split = ' ')[[1]],
      solve = strsplit(aCase[2], split = ' ')[[1]]
    )
  }
  )

  uniq.count <- sapply(puzzle, function(l){
    sum(sapply(l$solve, function(ges) nchar(ges)%in%uniq.lens))
  })
  sum(uniq.count)
}


# Part II ####
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

idx2term <- function(idx, upper=FALSE){
  paste0(if(upper) LETTERS[idx] else letters[idx], collapse = '')
}
#idx2term(c(3,4,1))

term2idx <- function(s){
  chars <- strsplit(tolower(s), split = '')[[1]]
  sapply(chars, function(chr) which(letters[1:7]==chr))
}
#term2idx('cdf')

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
#
signals2digits <- function(signals){
  # Find signals to segment transition
  sig.seg.mat <- crack(signals)
  # Find signals to segments
  sig2seg <-
    sapply(toupper(signals), # do this for all terms
           function(sig){
             paste0( # join the segments
               sort( # to get the correct order
                 sapply(term2idx(sig), # for all signals
                        function(j){ # find the transition to segment
                          letters[which(sig.seg.mat[j,]==1)]
                        })
               ),
               collapse = '')
           })
  # find signal to digits using segments
  sapply(sig2seg, function(seg) seg2dig[[seg]])
}

sig2num <- function(keyCode){
  # separate key and code
  key.code <- strsplit(keyCode, split=' | ', fixed=TRUE)[[1]]
  # separate keys
  key = strsplit(key.code[1], split = ' ')[[1]]
  # separate codes
  code = strsplit(key.code[2], split = ' ')[[1]]
  # find transition between signals and segments using keys
  sig.seg.mat <- crack(key)
  # find segments that each code is iluminating
  # Find signals to segments
  code2seg <-
    sapply(toupper(code), # do this for all terms
           function(sig){
             paste0( # join the segments
               sort( # to get the correct order
                 sapply(term2idx(sig), # for all signals
                        function(j){ # find the transition to segment
                          letters[which(sig.seg.mat[j,]==1)]
                        })
               ),
               collapse = '')
           })

  paste0(
    sapply(code2seg, function(sig) seg2dig[[sig]]),
    collapse = '')
}

sum(as.numeric(sapply(x, sig2num)))

