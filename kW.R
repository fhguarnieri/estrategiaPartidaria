#
# Acha k e W
#

library(tidyverse)
library(stringr)

DF <- read.csv("/home/iesp2/Documentos/Bancadas/TSE_DEPUTADO_FEDERAL_UF_CANDIDATO_COLIG.csv")

DFqe <- left_join(DF,qe, by = c("ANO_ELEICAO","UF","NUM_TURNO","DESCRICAO_CARGO"))
DFqe <- DFqe %>% arrange(ANO_ELEICAO,UF,SIGLA_PARTIDO,desc(QTDE_VOTOS))
DFqe <- left_join(DFqe,b,by = c("ANO_ELEICAO" = "ano", "SIGLA_PARTIDO" = "PARTIDO", "UF" = "variable"))
vleg <- DFqe %>% select(ANO_ELEICAO,UF,SIGLA_PARTIDO,QTDE_VOTOS,NUMERO_CANDIDATO) %>% filter(str_count(NUMERO_CANDIDATO) < 3)
DFqe <- DFqe %>% filter(str_count(NUMERO_CANDIDATO) > 2)
DFqe <- DFqe %>% group_by(ANO_ELEICAO,UF,SIGLA_PARTIDO) %>% mutate(ncands = n())
is.na(DFqe$SIGLA_COLIGACAO) <- DFqe$SIGLA_COLIGACAO == "#NULO#"
is.na(DFqe$SIGLA_COLIGACAO) <- DFqe$SIGLA_COLIGACAO == "#NE#"
DFqe <- DFqe %>% group_by(ANO_ELEICAO,UF,SIGLA_COLIGACAO) %>% mutate(ncolig = n())
DFqe <- DFqe %>% group_by(ANO_ELEICAO,UF,SIGLA_PARTIDO) %>% mutate(rk = row_number(desc(QTDE_VOTOS)))
DFqe <- DFqe %>% group_by(ANO_ELEICAO,UF,SIGLA_COLIGACAO) %>% mutate(rkcol = row_number(desc(QTDE_VOTOS)))
DFqe$ncolig <- ifelse(is.na(DFqe$SIGLA_COLIGACAO),DFqe$ncands,DFqe$ncolig)
DFqe$rkcol <- ifelse(is.na(DFqe$SIGLA_COLIGACAO),DFqe$rk,DFqe$rkcol)
DFqe <- DFqe %>% rename(vagas = Cadeiras)
DFqe <- DFqe %>% rename(cadeiras = value)
DFqe$cadeiras[is.na(DFqe$cadeiras)] <- 0
DFqe$alvo <- ifelse(DFqe$cadeiras == 0, 1, DFqe$cadeiras)
DFqe$maxCan <- ifelse(DFqe$TIPO_LEGENDA == "COLIGACAO", DFqe$vagas*2,DFqe$vagas*1.5)

L <- DFqe %>% filter(rk <= alvo)

L <- L %>% group_by(ANO_ELEICAO,UF,TIPO_LEGENDA,SIGLA_COLIGACAO,SIGLA_PARTIDO,maxCan,ncolig,ncands) %>% summarise(Vl = sum(QTDE_VOTOS), L = n())

L <- left_join(L,vleg,by = c("ANO_ELEICAO","UF","SIGLA_PARTIDO"))
L <- L %>% rename(Vleg = QTDE_VOTOS)
L <- left_join(L,qe,by = c("ANO_ELEICAO","UF"))
L <- L %>% mutate(Vp = Vl + Vleg)

L$k <- L$Vp/(L$QE*L$L)
L$k <- ifelse(L$k > 1,1,L$k)
L$W <- round(L$L/L$k)
L$W <- ifelse(L$W > L$maxCan, L$maxCan, L$W)
L$Wp <- L$W/L$maxCan
L$N <- ifelse(L$W > L$ncands, L$W - L$ncands,0)

Lc <- L[L$TIPO_LEGENDA == "PARTIDO_ISOLADO" | L$TIPO_LEGENDA == "PARTIDO ISOLADO",]
Lc$k <- Lc$Vp/(Lc$QE*Lc$L)
Lc$k <- ifelse(Lc$k > 1,1,Lc$k)
Lc$W <- round(Lc$L/Lc$k)
Lc$W <- ifelse(Lc$W > Lc$maxCan, Lc$maxCan, Lc$W)
Lc$Wp <- Lc$W/Lc$maxCan
Lc$N <- ifelse(Lc$W > Lc$ncands, Lc$W - Lc$ncands,0)

save(L,file = "L.RData")
