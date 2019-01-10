# installing/loading the package:
if(!require(installr)) {
  install.packages("installr"); require(installr)} #load / install+load installr

# using the package:
updateR() # this will start the updating process of your R installation.  It will check for newer versions, and if one is available, will guide you through the decisions you'd need to make.


devtools::install_github("brentthorne/posterdown")

# Load packages
library(ggplot2) # visualization
library(ggrepel)
library(ggthemes) # visualization
library(scales) # visualization
library(dplyr) # data manipulation
library(VIM)
library(data.table)
library(formattable)
library(plotly)
library(corrplot)
library(GGally)
library(caret)
library(car)



IMDB  <- read.csv("movie_metadata.csv")

# remove duplicate rows
sum(duplicated(IMDB))

# delete duplicate rows
IMDB <- IMDB[!duplicated(IMDB), ]%>%mutate(ROI = (gross-budget)/budget)

# tidy up movie title
library(stringr)
IMDB$movie_title <- gsub("?", "", as.character(factor(IMDB$movie_title)))
str_trim(IMDB$movie_title, side = "right")

summary(IMDB)
#IMDb Top 250 Directors - IMDb
#https://www.imdb.com/list/ls062419846/
# Tratar de descargar la IMBD score de los directores y cruzar esta data con la actual. 
# Codificar esta variable de forma binaria: 1- El director est? rankeado entre los top 250.
#                                           0- El director no pertenece al top ranking.
summary(IMDB$director_name)

hist(IMDB$duration, breaks = 100)
hist(IMDB$director_facebook_likes, breaks = 100)
hist(IMDB$actor_1_facebook_likes, breaks = 100)
hist(IMDB$gross, breaks = 100)
hist(IMDB$gross, breaks = 100)
hist(IMDB$budget, breaks = 100)


hist(IMDB$ROI, breaks = 200)

# create a new data frame
genres.df <- as.data.frame(IMDB[,c("genres", "ROI")])

# separate different genres into new columns
genre <- c("Action","Adventure","Animation","Biography","Comedy","Crime","Documentary","Drama","Family","Fantasy","Film-Noir","History","Horror","Musical","Mystery","News","Romance","Sci-Fi","Short","Sport","Thriller","War","Western","Music", "Game-Show", "Reality-TV")
ind <- sapply(1:length(genre), function(i)(sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% genre[i]) 1 else 0)))
colnames(ind) <- genre
genres.df <- data.frame(genres.df, ind)
dim(genres.df)

colSums(genres.df[,-c(1:2)])
rowSums(genres.df[,-c(1:2)])
length(which(rowSums(genres.df[,-c(1:2)])==1))
g <- unique(genres.df$genres[which(rowSums(genres.df[,-c(1:2)])==1)])


#Correlaci?n con los g?neros ?nicos
#16 variables
ggcorr(select(genres.df[which(rowSums(genres.df[,-c(1:2)])==1),-c(1:2)],str_replace(g, "-", ".")), label = TRUE, label_round = 2, label_size = 3.5, size = 2, hjust = .85) +
  ggtitle("Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))

#Correlaci?n con todos los g?neros
# 26 variables_ 10 modalidades a distribuir.
ggcorr(genres.df[,-c(1:2)], label = TRUE, label_round = 2, label_size = 3.5, size = 2, hjust = .85) +
  ggtitle("Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))

#ACP
library("FactoMineR")
library("factoextra")
res.pca <- PCA(genres.df[,-c(1:2)],  graph = FALSE)
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_var(res.pca, col.var = "black")
