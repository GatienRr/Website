# BONJOUR !
# Vous venez de télécharger un petit code pour faire un graphique des publications de N. Bourbaki
# C'est un début de travail que je partage au cas où ça peut intéresser quelqu'un
# Je n'ai pas commenté grand chose donc n'hésitez pas à m'écrire à ricotier@math.unistra.fr pour avoir plus
# d'informations.

rm(list=ls())


# Il peut y avoir des trucs à installer en utlisant install.packages("???")
library(readxl)
library(dplyr)
library(ggplot2)
library(rlist)
library(xlsx)
library(scales)
library(plotly)
# library(gdata)
# library(readr)
# library(timevis)
# library(timelineS)


getwd() # Pour regarder où est-ce qu'on est en train de travailler
# setwd("???") se placer dans le répertoire où ce trouve les données "Editions des elements.xlsx"

data.files = list.files(pattern = "*.xlsx") # Recherche tous les .xlsx du répertoire en cours
print(data.files)


# AU DEBUT C'EST UN AREA PAR LIVRE

data <- as.data.frame(read_excel("Editions_des_elements.xlsx"))
colnames(data)[1] <- "year"
colnames(data)
data
cumuleur <- function(df, noms, year, classement){
  # classement = "freq" ou classement = "firstyear"
  NomsUnique <- as.data.frame(unique(df[,noms]), stringAsFactor = FALSE)
  colnames(NomsUnique) <- "noms"
  NomsUnique$freq <- NA
  NomsUnique$firstyear <- NA
  cumul <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(cumul) <- c("year", "freq", "noms", "cumu")
  for (i in NomsUnique$noms) {iter <- grep( pattern = i, df[,noms])
  NomsUnique$freq[which(NomsUnique$noms==i)] <- length(iter)
  NomsUnique$firstyear[which(NomsUnique$noms==i)] <- df[iter[order(df[iter,year])[1]],year]
  NomsUnique <- NomsUnique[order(NomsUnique[,classement]),]
  Frequence <- as.data.frame(table(df[iter,year]), stringAsFactors=FALSE)
  colnames(Frequence)[1] <- "year"
  Frequence$noms <- df[iter[1],noms]
  Timeline <- data.frame(df[order(df[,year])[1],year]:df[order(df[,year])[nrow(df)],year],0)
  colnames(Timeline) <- c("year", "freq")
  Timeline$noms <- df[iter[1],noms]
  # plot = Frequence, Var1=year, allyear=Timeline
  for (i in Frequence$year) {Timeline$freq[which(Timeline$year == i)] <- Frequence$Freq[which(Frequence$year == i)]
  }
  # Pour chaque année je mets la fréquence qui va bien
  Timeline$cumu <- Timeline$freq
  for (i in 2:nrow(Timeline)) { Timeline$cumu[i] <- Timeline$cumu[i-1]+Timeline$cumu[i]}
  # Je fais le cumul
  cumul <- rbind(cumul, Timeline)
  }
  return(cumul)
}
dataplot <- cumuleur(df=data, noms="group", year="year", classement = "firstyear")
colnames(dataplot)

dataplot[order(dataplot$year),]




ggplot(dataplot, aes(x=year, y=cumu)) + geom_point(size=2, shape=23)
g <- ggplot(dataplot, aes(x=year, y=cumu)) + geom_area(aes(fill= noms), position = 'stack') 
g

datascale <- dataplot
datascale$year <- as.Date(paste0(dataplot$year,'-01-01'))
g <- ggplot(datascale, aes(x=year, y=cumu), axes=FALSE) + labs(x= "Année", y = "Publications du groupe Bourbaki") 
g <- g + scale_x_date(date_breaks = "5 year", date_labels = "%Y") + geom_area(aes(fill= noms), position = 'stack')
g + theme(legend.position ="bottom") + labs(fill="Livre") # + scale_fill_grey()
g
# Beau plot avec année scalé 
# Problèmes : ordre d'apparition des noms (c'est classé par ordre alphabétique !!!), on voit pas grand chose

# Pour le partage Web.. mais moins propre. J'améliorerai plus tard.
g <-ggplotly(g)
g


plotThirdEd <-cumuleur(df=data[which(data$Edition==3),], noms="group", year="year", classement = "firstyear")
plotThirdEd$year <- as.Date(paste0(plotThirdEd$year,'-01-01'))
g <- ggplot(plotThirdEd, aes(x=year, y=cumu), axes=FALSE) + labs(x= "Année", y = "Cumul") 
g + scale_x_date(date_breaks = "5 year", date_labels = "%Y") + geom_area(aes(fill= noms), position = 'stack')




dataed <- cumuleur(df=data, noms="Edition", year="year", classement = "firstyear")
colnames(dataplot)
dataed$year <- as.Date(paste0(dataed$year,'-01-01'))
g <- ggplot(dataed[order(dataed$noms, decreasing = T),], aes(x=year, y=cumu), axes=FALSE) + labs(x= "Année", y = "Cumul") 
g + scale_x_date(date_breaks = "5 year", date_labels = "%Y") + geom_area(aes(fill= noms), position = 'stack')
# Beau plot du cumul des éditions et articles
levels(dataed$noms)
dataed$noms <- as.factor(dataed$noms)
dataed$noms <- relevel(dataed$noms, "A")
dataed$noms <- relevel(dataed$noms, "2")
dataed$noms <- relevel(dataed$noms, "3")

g <- ggplot(dataed[order(dataed$noms, decreasing = T),], aes(x=year, y=cumu), axes=FALSE) + labs(x= "Année", y = "Publications du groupe Bourbaki") 
g <- g + scale_x_date(date_breaks = "5 year", date_labels = "%Y") + geom_area(aes(fill= noms), position = 'stack')
g + theme(legend.position ="bottom") + labs(fill="Numéros d'édition (et articles)") + scale_fill_grey()


# Ordre différent mais je sais pas si c'est mieux..

