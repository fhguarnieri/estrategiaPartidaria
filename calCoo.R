#
# Análise de variânicia do impacto dos partidos e dos candidatos
#

library(rethinking)
library(broom.mixed)


mapDesp <- dcast(extrUFfim, IBGEcod + municipio ~ numcand, value.var = "valor", fun.aggregate = sum)

# Examinando a variância

ef <- melt(mapDesp[1:15,-1])
ef <- ef %>% mutate(val = log10(value+1))
ef$partido <- substr(ef$variable,1,2)


post <- stan_lmer(val ~ 1 + (1|partido) + (1|variable),
                   data = ef, prior_intercept = cauchy(),
                   prior_covariance = decov(shape = 2, scale = 2),
                   adapt_delta = 0.999)
tidy(post,effects = "ran_pars")

# examinando matriz de adjacência

UF <- c("AC","AL","AP","AM","BA","CE","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")

results <- list()

for(uf in 1:length(UF)){
  load(paste0("/home/iesp2/Documentos/meusArtigos/abcp22/extr",UF[uf],".RData"))
  mapDesp <- dcast(extrUFfim, IBGEcod + municipio ~ numcand, value.var = "valor", fun.aggregate = sum)
  
  md <- mapDesp[complete.cases(mapDesp$IBGEcod),]
  
  tmp <- md[,3:dim(md)[2]]
  tmp[tmp > 0] <- 1
  z <- which(colSums(tmp) == 0)
  if(sum(z) > 0) tmp <- tmp[,-c(z)]
  #mx <- which.max(rowSums(tmp))
  #tmp <- tmp[-c(mx),]
  tmp <- as.matrix(tmp)
  #tmp <- tmp[-15,] # Tira a capital
  
  ng <- sum(rowSums(tmp) > 0) # numero de cidades do partido
  cog <- sum(rowSums(tmp) > 1) # numero de coocorrências
  pg <- cog/ng # COMPETIÇÃo INTER PARTIDÁRIA
  
  
  partidos <- substring(colnames(tmp),1,2)
  part <- names(table(partidos)[table(partidos) > 1])
  
  compintra <- data.frame("partido" = NA,"p" = NA, "co" = NA, "nm" = NA, "ncand" = NA, "ale" = NA)
  
  for(i in 1:length(part)){
    # seleciona partido
    tp <- tmp[,substring(colnames(tmp),1,2) %in% part[i]]
    n <- sum(rowSums(tp) > 0) # numero de cidades do partido
    co <- sum(rowSums(tp) > 1) # numero de coocorrências
    cop <- co/n
    ncand <- dim(tp)[2]
    
    # seleciona candidatos aleatoriamente para 'partido'
    psim <- data.frame("n" = NA, "co" = NA, "cops" = NA)
    nsims <- 10000
    for(j in 1:nsims){
      sp <- sample(1:dim(tmp)[2],dim(tp)[2]) # cria uma amostra de candidatos do tamanho da lista do partido
      ts <- tmp[,sp]
      ns <- sum(rowSums(ts) > 0, na.rm = T) # numero de cidades do partido
      cos <- sum(rowSums(ts) > 1, na.rm = T) # numero de coocorrências
      cops <- cos/ns
      psim[j,] <- c(ns, cos, cops)
    }
    
    op <- psim$cops[order(psim$cops)]
    p <- sum(op <= cop, na.rm = T)/length(op)
    ale <- mean(psim$cops)
    compintra[i,] <- c(part[i],round(p,2),co,n,ncand, ale)
  }
  
  
  #results <- list("AC",pg,compintra)
  
  results <- append(results, list(UF[uf],pg,compintra))
  
}

save(results, file = "results2.RData")
rold <- r1
r1 <- data.frame("partido" = NA, "p" = NA, "co" = NA, "nm" = NA, "ncand" = NA, "uf" = NA)
for(i in seq(3,length(results),by = 3)){
  uf <- rep(UF[i/3],nrow(results[[i]]))
  r1 <- rbind(r1, cbind(results[[i]],uf)) # extraindo coocorrências
}
r1 <- r1[-1,]
r1 <- r1[order(r1$partido, r1$p),]

r1$p <- as.numeric(r1$p)
r1$co <- as.numeric(r1$co)
r1$nm <- as.numeric(r1$nm)
r1$ncand <- as.numeric(r1$ncand)


r1 <- r1 %>% mutate(pco = ifelse(co > 0, (co-1)/nm,0))
ggplot(r1) + geom_histogram(aes(pco)) + labs(x = "proporção de coocorrências", y = "contagem")
ggplot(r1) + geom_boxplot(aes(y = pco)) + labs(y = "proporção de coocorrências")
ggplot(r1, aes(uf,pco)) + geom_boxplot() + labs(y = "proporção de coocorrências")

pufmedio <- r1 %>% group_by(uf) %>% summarise(mp = median(pco))
pufmedio <- inner_join(pufmedio,vagas,by = c("uf" = "UF"))

ggplot(pufmedio, aes(Cadeiras,mp)) + geom_point() + geom_smooth()

ggplot(r1, aes(partido,pco)) + geom_boxplot() + labs(y = "proporção de coocorrências")

pmedio <- r1 %>% group_by(partido,ncand) %>% summarise(mp = median(pco))

ggplot(pmedio, aes(ncand,mp)) + geom_point() + geom_smooth(method = lm)


ggplot(pmedio, aes(y = mp)) +
  geom_boxplot() +
  labs(y = "mediana da proporção de coocorrências") +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())

r1 <- r1 %>% left_join(vagas,by = c("uf" = "UF"))

ggplot(r1) + geom_histogram(aes(p)) + labs(x = "probabilidade", y = "contagem")
#ggplot(r1, aes(uf,p)) + geom_boxplot() + labs(y = "probabilidade")
ggplot(r1, aes(partido,p)) + geom_boxplot() + labs(y = "probabilidade")

inter <- c()
for(u in seq(2,length(results),by = 3)) inter[(u %/% 3)+1] <- results[[u]]
inter <- as.numeric(inter)

plot(density(pmedio$mp, adjust = 1),
     xlim = c(-0.2,1.2), ylim = c(0,3.5),
     main = "", xlab = "Proporção de coocorrência",
     col = "blue")
lines(density(inter,adjust = 1), col = "green")
legend(-0.2,3.5,c("Intra","Inter"), fill = c("blue","green"), cex = 0.8)

save(r1, file = "r1.RData")



