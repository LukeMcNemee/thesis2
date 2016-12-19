require(matrixStats) #install.packages('matrixStats')
setwd("~/Dokumenty/studijni_materialy/Edu-hoc/src")

dat1 = read.delim("RssiApp/values6.txt", header = FALSE, sep = ";", comment.char = "&")
dat2 = read.delim("RssiApp2/values6.txt", header = FALSE, sep = ";", comment.char = "&")
dat3 = read.delim("RssiAppSniffer/values6.txt", header = FALSE, sep = ";", comment.char = "&")

basicStats <- function(x){
  m = mean(x)
  sd = sd(x)
  
  qp = m + alpha * sd
  qm = m - alpha * sd
  
  result <- list("mean"=m, "sd"=sd, "qp"=qp, "qm"=qm)
  return(result)
}

quantization <- function(stats, values){
  y = c(NA)
  x = c(NA)
  for(i in  1:length(values)){
    if(values[i] > stats$qp){
      y <- c(y,1)
      x <- c(x,i)
    } else if (values[i] < stats$qm){
      y <- c(y,0)
      x <- c(x,i)
    }
  }
  return(list("x"=na.omit(x),"y"=na.omit(y)))
}

mapLocations <- function(locations, bits){
  result = c(NA)
  
  for(i in locations){
    result = c(result, bits$y[which(bits$x == i)])
  }
  return(na.omit(result))
}

inc <- function(x){
  eval.parent(substitute(x <- x + 1))
}

distance <- function( a, b){
  d <- 0;
  if(length(a) == 0 || length(b) == 0){
    return(max(length(a), length(b)))
  }
  a <- na.omit(a)
  b <- na.omit(b)
  if(length(a) <= length(b)){
    for(i in 1:length(a)){
      if(a[i] != b[i]){
        inc(d)
      }
    }
  } else {
    for(i in 1:length(b)){
      if(a[i] != b[i]){
        inc(d)
      }
    }
  }
  return(d)
}

lengths <- c(NA)
h  <- c(NA)
hS <- c(NA)

alpha <- 0.5
for (i in 1:140){ 
#i<-1
  values1 <- as.numeric(dat1[i,1:1000]) -256
  values2 <- as.numeric(dat2[i,1:1000]) -256
  values3 <- as.numeric(dat3[i,1:1000]) -256
  
  s1 <- basicStats(values1)
  r1 <- quantization(s1, values1)
  
  s2 <- basicStats(values2)
  r2 <- quantization(s2, values2)
  
  s3 <- basicStats(values3)
  r3 <- quantization(s3, values3)
  
  #plot(r1$x, r1$y, col = "red", pch=0,  ylim=c(-0.1,1.1))
  #points(r2$x, r2$y+0.02, col = "blue", pch=1)
  #points(r3$x, r3$y+0.04, col = "green", pch=2)
  
  
  x_locations = intersect(r1$x,r2$x)
  s_locations = intersect(x_locations,r1$x)
  
  bits1 <- mapLocations(x_locations, r1)
  bits2 <- mapLocations(x_locations, r2)
  bits3 <- mapLocations(s_locations, r3)
  
  lengths <- c(lengths, length(bits1))
  if(length(bits1) != 0){
    h <- c(h, distance(bits1, bits2))
    hS <- c(hS, distance(bits1, bits3))
  }
  
  if(length(bits1) > 5){
    sink("outfile.txt", append=TRUE)
    for(i in 1:length(bits1)){
      cat(bits1[i])
    }
    cat("\n")
    sink()
  }
  
  #points(x_locations, bits1 -0.02, col = "red", pch=15)
  #points(x_locations, bits2 -0.04, col = "blue", pch=16)
  #points(s_locations, bits3 -0.06, col = "green", pch=17)

}
hist(h, breaks = 50, main = "histogram of distances for non zero vectors between A and B", col = "grey")
hist(hS, breaks = 50, main = "histogram of distances for non zero vectors between AB and sniffer", col = "grey")
hist(lengths, breaks = 50, main = "histogram of lenghts of final vectors", col = "grey")

boxplot(h, hS, names=c("distances between A and B","distances between AB and Sniffer"))
  
print(table(lengths))