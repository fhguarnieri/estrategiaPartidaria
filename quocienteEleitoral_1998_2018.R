#
# Calcula QE
#

qe <- read.csv("/home/iesp2/Documentos/Bancadas/TSE_DEPUTADO_FEDERAL.csv")

qe$VV <- qe$QT_VOTOS_NOMINAIS+qe$QT_VOTOS_LEGENDA

vagas <- data.frame("UF" = c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA",
                             "MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN",
                             "RO","RR","RS","SC","SE","SP","TO"),
                    "Cadeiras" =  c(8,9,8,8,39,22,8,10,17,18,53,8,8,17,12,25,10,
                                    30,46,8,8,8,31,16,8,70,8))

qe <- inner_join(qe,vagas,by = "UF")

qe$QE <- qe$VV/qe$Cadeiras
qe$QE <- round(qe$QE)
