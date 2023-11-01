#
# Testando hipóteses
#

library(tidyverse)
library(reshape2)


# H1 calcula k e verifica correlação entre W (padronizado) em t+1 e k em t

lcast <- dcast(Lc, UF + SIGLA_PARTIDO ~ ANO_ELEICAO, value.var = "k", fun.aggregate = length)
kcast <- dcast(Lc, UF + SIGLA_PARTIDO ~ ANO_ELEICAO, value.var = "k", fun.aggregate = mean)
Wcast <- dcast(Lc, UF + SIGLA_PARTIDO ~ ANO_ELEICAO, value.var = "Wp", fun.aggregate = mean)

plot(kcast[which(lcast$'1998'+lcast$'2002' == 2),"1998"],log(Wcast[which(lcast$'1998'+lcast$'2002' == 2),"2002"]+1))
plot(kcast[which(lcast$'2002'+lcast$'2006' == 2),"2002"],log(Wcast[which(lcast$'2002'+lcast$'2006' == 2),"2006"]+1))
plot(kcast[which(lcast$'2006'+lcast$'2010' == 2),"2006"],log(Wcast[which(lcast$'2006'+lcast$'2010' == 2),"2010"]+1))
plot(kcast[which(lcast$'2010'+lcast$'2014' == 2),"2010"],log(Wcast[which(lcast$'2010'+lcast$'2014' == 2),"2014"]+1))
plot(kcast[which(lcast$'2014'+lcast$'2018' == 2),"2014"],log(Wcast[which(lcast$'2014'+lcast$'2018' == 2),"2018"]+1))

cor.test(kcast[which(lcast$'1998'+lcast$'2002' == 2),"1998"],log(Wcast[which(lcast$'1998'+lcast$'2002' == 2),"2002"]+1))
cor.test(kcast[which(lcast$'2002'+lcast$'2006' == 2),"2002"],log(Wcast[which(lcast$'2002'+lcast$'2006' == 2),"2006"]+1))
cor.test(kcast[which(lcast$'2006'+lcast$'2010' == 2),"2006"],log(Wcast[which(lcast$'2006'+lcast$'2010' == 2),"2010"]+1))
cor.test(kcast[which(lcast$'2010'+lcast$'2014' == 2),"2010"],log(Wcast[which(lcast$'2010'+lcast$'2014' == 2),"2014"]+1))
cor.test(kcast[which(lcast$'2014'+lcast$'2018' == 2),"2014"],log(Wcast[which(lcast$'2014'+lcast$'2018' == 2),"2018"]+1))

# Relação entre W e ncolig


nt0 <- dcast(L, UF + SIGLA_PARTIDO ~ ANO_ELEICAO, value.var = "W", fun.aggregate = mean)
nt1 <- dcast(L, UF + SIGLA_PARTIDO ~ ANO_ELEICAO, value.var = "ncolig", fun.aggregate = mean)

tt <- rowSums(lcast[,3:8])

nt0 <- nt0[tt>1,]
nt1 <- nt1[tt>1,]

nt0 <- melt(nt0)
nt0 <- nt0 %>% filter(variable == 1998)
nt0 <- nt0[order(nt0$UF,nt0$SIGLA_PARTIDO,nt0$variable),]

nt1 <- melt(nt1)
nt1 <- nt1 %>% filter(variable == 1998)
nt1 <- nt1[order(nt1$UF,nt1$SIGLA_PARTIDO,nt1$variable),]
nt1 <- nt1[-1,]
nt1[5406,] <- rep(NA,4)

nt0$SIGLA_PARTIDO_t1 <- nt1$SIGLA_PARTIDO
nt0$variable_t1 <- nt1$variable
nt0$value_t1 <- nt1$value

nn <- nt0$SIGLA_PARTIDO == nt0$SIGLA_PARTIDO_t1

nt0 <- nt0[nn,]

summary(lm(nt1$value~nt0$value))
