# Tutorial paquet parsec
library(parsec)
library(tidyverse)
load("C:\\Users\\franc\\OneDrive - IQS\\IQS\\Recerca\\TFCs\\2023_Pau_Lluch\\DadesPOSETs.rdata")

perfil <- perfil01_02_2023
var_color <- perfil$tipus_estacio

perfil <- perfil %>% select(-SO2, -O3)
perfil <- perfil %>% drop_na()

rownames(perfil) <- perfil$codi_eoi
rownames(perfil) <- substr(rownames(perfil), 3, 25)
X <- rownames(perfil)
perfil$codi_eoi <- NULL
perfil_num <- perfil %>% select(-tipus_estacio, -area_urbana)

# Càlcul del poset
Z <- get_poset(perfil_num, X)

# Average heights
Z.avg <- average_ranks(Z)
plot.average_ranks(Z.avg)
lingen(Z)

maximal(Z)
minimal(Z)

incomp(Z) # Matriu d'incomparabilitats

LEZ <- LE(Z)
