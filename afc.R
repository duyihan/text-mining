setwd("~/EISTI/txt mining/harry")


library(xlsx)

#regionales.data <- read.xlsx(file="mm.xlsx",header=T,sheetIndex=1,row.names=1)
#regionales.data <- read.xlsx(file="foods.xls",header=T,sheetIndex=1,row.names=1)
#regionales.data <- read.xlsx(file="Regionales.xlsx",header=T,sheetIndex=1,row.names=1)
regionales.data <- read.xlsx(file="~/EISTI/txt mining/harry/harry.xlsx",header=T,sheetIndex=2,row.names=1)

#Tableau de contingence 

df<-regionales.data/sum(regionales.data)*100

#load(file.choose()) # Choisir le fichier Regionales-2004-IDF.Rdata

library(FactoMineR)
regionales.CA <- CA(regionales.data, ncp=3)
summary(regionales.CA)

#Les valeurs propres

regionales.CA$eig


#Les r??sultats relatifs aux lignes : coordonn??es, contributions et qualit??s de repr??sentation

regionales.CA$row

#Les r??sultats relatifs aux colonnes : coordonn??es, contributions et qualit??s de repr??sentation

regionales.CA$col

#Graphique : repr??sentation conjointe lignes et colonnes dans le plan (CP1, CP2), pour une installation de R o?? le device graphique "png" est disponible :

png("AFC-avec-R-Gra1.png")
plot.CA(regionales.CA, axes=c(1,2))
dev.off()


#De m??me : repr??sentation graphique conjointe lignes et colonnes dans le plan (CP2, CP3) :

png("AFC-avec-R-Gra2.png")
plot.CA(regionales.CA, axes=c(2,3))
dev.off()








#Une autre repr??sentation graphique int??ressante peut ??tre obtenue ?? l'aide de la fonction table.value du package ade4 :

#library(ade4)
#png("/Users/carpenti/Desktop/AFC-avec-R-Gra3.png")
#table.value(regionales.data, grid=T)
#dev.off()


#regionales.coa <- dudi.coa(regionales.data)
#Select the number of axes: 3
#options(digits=3)
#regionales.coa$tab


regionales.CA$call$marge.col
