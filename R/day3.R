# day 3
x <- read.table(file='input', header = FALSE,
                as.is = TRUE, colClasses = 'character')
x <- x$V1
n <- nchar(x)
N <- max(n)
y <- matrix(0, nrow = length(n), ncol = N)
tmp <- sapply(1:length(n),
       function(i)
         sapply(1:n[i],
                function(k) {
                  t <- as.integer(substr(x[i], n[i]-k+1, n[i]-k+1))
                  if(is.na(t)) print(paste('Error in ', i, 'and', k))
                  else y[i,N-k+1] <<- t
                }
         )
)

Y <- colSums(y)
gamma_rate <- sum((1*(Y>(length(n)/2))) * (2^((N:1)-1)))
epsilon_rate <- sum((1*(Y<(length(n)/2))) * (2^((N:1)-1)))
gamma_rate*epsilon_rate

# Part 2
# Most common digits
mcd <- function(x){
  if(dim(x)[1]==1){
    return(x[1,])
  } else{
    #print(dim(x))
    if(sum(x[,1])>=dim(x)[1]/2)
      return(c(1, mcd(x[x[,1]==1,-1, drop=FALSE])))
    else
      return(c(0, mcd(x[x[,1]==0,-1, drop=FALSE])))
  }
}
oxy <- mcd(y)


# Least common digits
lcd <- function(x){
  if(dim(x)[1]==1){
    return(x[1,])
  } else{
    #print(dim(x))
    if(sum(x[,1])>=dim(x)[1]/2)
      return(c(0, lcd(x[x[,1]==0,-1, drop=FALSE])))
    else
      return(c(1, lcd(x[x[,1]==1,-1, drop=FALSE])))
  }
}
co2 <- lcd(y)

sum(oxy*(2^((N:1)-1)))*sum(co2*(2^((N:1)-1)))


