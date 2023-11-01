#
# Dispersão despesas 2018 BR
#

library(tidyverse)
library(stringdist)
library(rgdal)
library("maptools")
library(broom)
library(sf)
library(gridExtra)
library(reshape2)
library(tmap)


load("/home/iesp2/Documentos/meusArtigos/abcp22/cidadesDF.RData")
load("/home/iesp2/Documentos/meusArtigos/abcp22/cidades.RData")
load("/home/iesp2/Documentos/meusArtigos/abcp22/totVot.RData")


UF <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")
E <- UF
uf <- 9


for(uf in 10:length(UF)){
  load(paste0("/home/iesp2/Documentos/meusArtigos/abcp22/extr",UF[uf],".RData"))
  
  #################### MAPAS #######################################################################
  
  
  mapDesp <- dcast(extrUFfim, IBGEcod + municipio ~ numcand, value.var = "valor", fun.aggregate = sum)
  
  
  cidades <- readOGR("/home/iesp2/Documentos/limongi/referendo2005/55mu1000gc/", "55mu1000gc", encoding = "Latin1")
  cidades <- subset(cidades, UF == E[uf]) # MUDAR!!!
  cidades@data$id = rownames(cidades@data)
  cidades@data <- merge(cidades@data, mapDesp, by.x = "GEOCODIGO", by.y = "IBGEcod", all.x = T)
  cidades@data <- cidades@data[order(as.numeric(cidades@data$id)),]
  cidades@data <- cidades@data[!duplicated(cidades@data$id),]
  cidades@data$tdesp <- rowSums(cidades@data[,13:length(names(cidades@data))], na.rm=T)
  cidades.points = st_as_sf(cidades, region="id")
  
  # distribuições de todas as transações
  ltdesp <- log10(cidades.points$tdesp +1)
  cidades.points <- cidades.points %>% mutate(ltdesp = ltdesp)
  tm_shape(cidades.points) + 
    tm_borders() + 
    tm_fill(col = "ltdesp", title = "log10(volume de transações - R$)")
  
  # transações por candidato
  
  idCand <- names(cidades.points[13:(dim(cidades.points)[2]-3)])
  for(id in idCand[1:(length(idCand)-1)]){
    lcand <- log10(cidades.points[[id]] +1)
    cidades.points$lcand <- lcand
    png(paste0("/home/iesp2/Documentos/meusArtigos/estrategiaPartidaria/Mapas/",UF[uf],"/",UF[uf],"_",id,".png"))
    print(
      tm_shape(cidades.points) + 
        tm_borders() + 
        tm_fill(col = "lcand", title = "log10(volume de transações - R$)") +
        tm_layout(id,
                  title.position = c("center","top"),
                  legend.title.size = 0.6,
                  legend.text.size = 0.6,
        )
    )
    dev.off()
  }
  
  
}
