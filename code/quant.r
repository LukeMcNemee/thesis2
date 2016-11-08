require(matrixStats)
setwd("~/Dokumenty/studijni_materialy/Edu-hoc/src")

dat1 = read.delim("RssiApp/values.txt", header = FALSE, sep = ";", comment.char = "&")

dat2 = read.delim("RssiApp2/values.txt", header = FALSE, sep = ";", comment.char = "&")
datS = read.delim("RssiAppSniffer/values.txt", header = FALSE, sep = ";", comment.char = "&")

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

alpha <- 0.5
#for (i in 1:423){ 
i<-1
  values1 <- as.numeric(dat1[i,1:1000]) -256
  #m1 = mean(values1)
  #sd1 = sd(values1)
  
  #qp1 = m1 + alpha * sd1
  #qm1 = m1 - alpha * sd1
  
  s1 <- basicStats(values1)
  
  m1 <- s1[1]
  
  y1 = c(NA)
  x1 = c(NA)
  for (j in 1:1000){
    if(values1[j] > qp1){
      y1 <- c(y1,1)
      x1 <- c(x1,j)
    } else if (values1[j] < qm1){
      y1 <- c(y1,0)
      x1 <- c(x1,j)
    } else {
      #bits <- c(bits,0)
    }
  }
  
  values2 <- as.numeric(dat2[i,1:1000]) -256
  m2 = mean(values2)
  sd2 = sd(values2)
  
  qp2 = m2 + alpha * sd2
  qm2 = m2 - alpha * sd2
  
  y2 = c(NA)
  x2 = c(NA)
  for (j in 1:1000){
    if(values2[j] > qp2){
      y2 <- c(y2,1)
      x2 <- c(x2,j)
    } else if (values2[j] < qm2){
      y2 <- c(y2,0)
      x2 <- c(x2,j)
    } else {
      #bits <- c(bits,0)
    }
  }
  
  
  valuesS <- as.numeric(datS[i,1:1000]) -256
  mS = mean(valuesS)
  sdS = sd(valuesS)
  
  qpS = mS + alpha * sdS
  qmS = mS - alpha * sdS
  
  yS = c(NA)
  xS = c(NA)
  for (j in 1:1000){
    if(valuesS[j] > qpS){
      yS <- c(yS,1)
      xS <- c(xS,j)
    } else if (values2[j] < qmS){
      yS <- c(yS,0)
      xS <- c(xS,j)
    } else {
      #bits <- c(bits,0)
    }
  }
  
  plot(na.omit(x1), na.omit(y1), col = "red", pch=0,  ylim=c(-0.1,1.1))
  points(na.omit(x2), na.omit(y2)+0.02, col = "blue", pch=1)
  points(na.omit(xS), na.omit(yS)+0.04, col = "green", pch=2)
  
  
  xres = intersect(x1,x2)
  xresS = intersect(x1,xS)
  yres1 = c(NA)
  yres2 = c(NA)
  yresS = c(NA)
  for(j in xres){
    yres1  = c(yres1, y1[which(x1 == j)])
    yres2  = c(yres2, y2[which(x2 == j)])
    
  }
  
  for(j in xresS){
    yresS  = c(yresS, yS[which(xS == j)])
  }
  points(na.omit(xres), na.omit(yres1) -0.02, col = "red", pch=15)
  points(na.omit(xres), na.omit(yres2) -0.04, col = "blue", pch=16)
  points(na.omit(xresS), na.omit(yresS) -0.06, col = "green", pch=17)
  
#}
