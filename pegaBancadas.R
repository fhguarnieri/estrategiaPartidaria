#
# Pegando bancadas
#

library(reshape2)

b98 <- read.csv2("bancadasEleicoes_1988.csv")
b98 <- melt(b98)
b98$ano <- rep(1998,nrow(b98))

b02 <- read.csv2("bancadasEleicoes_2002.csv")
b02 <- melt(b02)
b02$ano <- rep(2002,nrow(b02))

b06 <- read.csv2("bancadasEleicoes_2006.csv")
b06 <- melt(b06)
b06$ano <- rep(2006,nrow(b06))

b10 <- read.csv2("bancadasEleicoes_2010.csv")
b10 <- melt(b10)
b10$ano <- rep(2010,nrow(b10))

b14 <- read.csv2("bancadasEleicoes_2014.csv")
b14 <- melt(b14)
b14$ano <- rep(2014,nrow(b14))

b18 <- read.csv2("bancadasEleicoes_2014.csv")
b18 <- melt(b18)
b18$ano <- rep(2018,nrow(b18))

names(b98) <- names(b02) <- names(b06)
b <- rbind(b98,b02,b06,b10,b14,b18)


