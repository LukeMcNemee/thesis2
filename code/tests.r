setwd("~/Dokumenty/studijni_materialy/DP/data")

chunks <- function(d, n){      
  chunks <- split(d, ceiling(seq_along(d)/n))
  names(chunks) <- NULL
  return(chunks)
}

s2chunk2int <- function(ch){
  ch <- unlist(ch)
  if(length(ch)!= 2){
    #print("not size")
    #print(ch)
    return()
  }
  num <- ch[1]*2^0+ch[2]*2^1
  return(num)
}

s4chunk2int <- function(ch){
  ch <- unlist(ch)
  if(length(ch)!= 4){
    #print("not size")
    #print(ch)
    return()
  }
  num <- ch[1]*2^0+ch[2]*2^1+ch[3]*2^2+ch[4]*2^3
  return(num)
}

s8chunk2int <- function(ch){
  ch <- unlist(ch)
  if(length(ch)!= 8){
    #print("not size")
    #print(ch)
    return()
  }
  num <- ch[1]*2^0+ch[2]*2^1+ch[3]*2^2+ch[4]*2^3 + ch[5]*2^4+ch[6]*2^5+ch[7]*2^6+ch[8]*2^7
  return(num)
}


minEntropy <- function(cMax, N, W){
  pMax <- cMax/N
  cBound <- cMax + 2.3*sqrt(N * pMax*(1 - pMax))
  H <- -log2(cBound/N)
  return(min(W,H))
}

fileName <- "rotating.txt"
conn <- file(fileName,open="r")
linn <-readLines(conn)
#data <- c(unlist(strsplit(linn[1], split="")))
ones <- c(NA)
zeros <- c(NA)
lines = 0
for (i in 1:length(linn)){
  #data <- c(data,unlist(strsplit(linn[i], split="")))
  line <- as.numeric(unlist(strsplit(linn[i], split="")))
  if("1" %in% line){
    lines <- lines + 1
    ch2 <- chunks(line,2)
    ch4 <- chunks(line,4)
    ch8 <- chunks(line,8)
    numbers2 <- c(NA)
    numbers4 <- c(NA)
    numbers8 <- c(NA)
    for(i in ch2){
      numbers2 <- c(numbers2,s2chunk2int(i))
    }
    for(i in ch4){
      numbers4 <- c(numbers4,s4chunk2int(i))
    }
    for(i in ch8){
      numbers8 <- c(numbers8,s8chunk2int(i))
    }
    print(table(numbers8))
  }
  split(linn, ceiling(seq_along(linn)/4))
  hist(line)
  #print(mean(line))
  ones <- c(ones,sum(line == 1))
  zeros <- c(zeros,sum(line == 0))
}
close(conn)

#print(ones)
#print(zeros)

#data <- as.numeric(data)
