#Code of delicious
#DU Yihan
#CHENG Yueyao

library(datasets)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(taRifx)
library(igraph)
library('Matrix')

setwd("~/EISTI/Analyse-des-reseaux-sociaux/projet/delicious")


#read data
bookmark                         <- read.table("bookmarks.dat", header = TRUE, sep="\t", quote = "", fill=TRUE)
bookmark_tag                     <- read.table("bookmark_tags.dat", header = TRUE, sep="\t", quote = "", fill=TRUE)
tags                             <- read.table("tags.dat", header = TRUE, sep="\t", quote = "", fill=TRUE)
user_contacts_timestamps         <- read.table("user_contacts_timestamps.dat", header = TRUE, sep="\t", quote = "", fill=TRUE)
user_contacts                    <- read.table("user_contacts.dat", header = TRUE, sep="\t", quote = "", fill=TRUE)
user_taggedbookmarks_timestamps  <- read.table("user_taggedbookmarks_timestamps.dat", header = TRUE, sep="\t", quote = "", fill=TRUE)
user_taggedbookmarks             <- read.table("user_taggedbookmarks.dat", header = TRUE, sep="\t", quote = "", fill=TRUE)


#exploitation des donn??es

#choisir 500 bookmarks et supprimer les autres
user_taggedbookmarks<-user_taggedbookmarks[user_taggedbookmarks$bookmarkID<501,]
bookmark<-bookmark[bookmark$id<501,]


#convert data frame to graph
g_bookmark <- graph(bookmark$id)
g_tag <- graph(tags$id)
g_user <- graph.data.frame(user_contacts,directed = FALSE)

df_bookmark_tag <- data.frame(user_taggedbookmarks$tagID, user_taggedbookmarks$bookmarkID)
df_bookmark_user <- data.frame(user_taggedbookmarks$userID, user_taggedbookmarks$bookmarkID)
df_user_tag <- data.frame(user_taggedbookmarks$tagID, user_taggedbookmarks$userID)


#count the number of users, the tags and the bookmark
n_user<-vcount(g_user)
n_tag<-vcount(g_tag)
n_bookmark<-nrow(bookmark)

n_bookmark_tag<-nrow(df_bookmark_tag)
n_bookmark_user<-nrow(df_bookmark_user)
n_user_tag<-nrow(df_user_tag)




m_bookmark_tag<- Matrix(0, nrow = n_tag, ncol = n_bookmark, sparse=TRUE)
m_bookmark_user<- Matrix(0, nrow = n_user, ncol = n_bookmark, sparse=TRUE)
m_user_tag<- Matrix(0, nrow = n_tag, ncol = n_user, sparse=TRUE)


for (i in 1:n_bookmark_tag){
  if(df_bookmark_tag[i,1]<=n_tag){
    if(df_bookmark_tag[i,2]<=n_bookmark){
      m_bookmark_tag[df_bookmark_tag[i,1],df_bookmark_tag[i,2]]<-1
    }
  }
}
#============================================================
#calculate the community of bookmark by tags
tm_bookmark_tag<-t(m_bookmark_tag)
m_bookmark_tag<-tm_bookmark_tag %*% m_bookmark_tag 

seuil<-0
m_bookmark_tag[m_bookmark_tag<=seuil]<- 0
m_bookmark_tag[m_bookmark_tag>seuil]<- 1

g_final_bookmarkbytag <- graph_from_incidence_matrix(m_bookmark_tag)

communaute_bookmark_walk<-cluster_walktrap(g_final_bookmarkbytag)
communaute_bookmark_lou<-cluster_louvain(g_final_bookmarkbytag)
plot(communaute_bookmark_walk,g_final_bookmarkbytag,vertex.label=NA,vertex.size=0.1)
plot(communaute_bookmark_lou,g_final_bookmarkbytag,vertex.label=NA,vertex.size=0.1)


for (i in 1:n_bookmark_user){
  if(df_bookmark_user[i,1]<=n_user){
    if(df_bookmark_user[i,2]<=n_bookmark){
      m_bookmark_user[df_bookmark_user[i,1],df_bookmark_user[i,2]]<-1
    }
  }
}
#============================================================
#calculate the community of bookmark by user
tm_bookmark_user<-t(m_bookmark_user)
m_bookmarkbyuser<-tm_bookmark_user %*% m_bookmark_user
seuil<-0
m_bookmarkbyuser[m_bookmarkbyuser<=seuil]<- 0
m_bookmarkbyuser[m_bookmarkbyuser>seuil]<- 1


g_final_bookmarkbyuser <- graph_from_incidence_matrix(m_bookmarkbyuser)
communaute_bookmarkbyuser_walk<-cluster_walktrap(g_final_bookmarkbyuser)
communaute_bookmarkbyuser_lou<-cluster_louvain(g_final_bookmarkbyuser)

plot(communaute_bookmarkbyuser_walk,g_final_bookmarkbyuser,vertex.label=NA,vertex.size=0.1)
plot(communaute_bookmarkbyuser_lou,g_final_bookmarkbyuser,vertex.label=NA,vertex.size=0.1)


#============================================================
#calculate the community of user_contacts 
communaute_user_walk<-cluster_walktrap(g_user)
plot(communaute_user_walk,g_user,vertex.label=NA,vertex.size=0.1)
communaute_user_lou<-cluster_louvain(g_user)
plot(communaute_user_lou,g_user,vertex.label=NA,vertex.size=0.1)




### create a new data ####
user_taggedbookmarks   <- read.delim("user_taggedbookmarks.dat", header = TRUE, sep="\t")
bookmark               <- read.table("bookmarks.dat", header = TRUE, sep="\t", quote = "", fill=TRUE)

#delete bookmark > 5000
BG<-user_taggedbookmarks[user_taggedbookmarks$bookmarkID<5001,]
bookmark_tag<-bookmark_tag[bookmark_tag$bookmarkID<5001,]
bookmark<-bookmark[bookmark$id<5001,]


### add value of tags -----BG ####

tagname = c()
i <- 1
for(ins in BG$tagID){
  
  t=which(tags$id == ins)
  tagname<-c(tagname,as.character(tags$value[t]))
  
  i <- i+1
}
tagname <- as.data.frame(tagname)
BG <- data.frame(BG,tagname)


### Data cleaning 

# Remove instances which have at least one NA variable
BG <- BG[complete.cases(BG), ]
# Remove instances which are duplicated (duplicated based on bookmark)
#BG <- BG[!duplicated(BG$bookmarkID),]





### Tags Analysis ####

### Most popular Tags --number####

BG0 <- BG[BG$tagname != "", ]
tag <- c()
i <- 1
for (ins in BG0$tagname){
  pt <- strsplit(ins, "[|]")     #pt: principal tags
  if (length(pt) != 0){
    for (word in pt[[1]]){
      if (!(word %in% tag)){
        tag[i] <- word
        i = i + 1
      }
    }
  }
}
# Create a dataframe with logical values which 

# indiacte the tag of each bookmark
tag_idx <- BG0[, c("bookmarkID", "tagname")]
i = 1
mat <- matrix(rep(0, (dim(BG0)[1] * length(tag))), nrow = dim(BG0)[1])
for (word in tag_idx$tagname){
  idx <- which(tag %in% word)
  mat[i, idx] <- 1
  i = i + 1
}
colnames(mat) <- tag
bookmark_and_tag <- data.frame(mat)


# Find how many bookmarks belong in each tag
sum <- rep(0, length(tag))
for (i in 1:length(tag)){
  sum[i] <- sum(bookmark_and_tag[, i])
}

tag_sum <- data.frame(tag,sum)
tag_sum <- tag_sum[order(sum, decreasing = FALSE),]
tag_sum$tag <- factor(tag_sum$tag, levels = tag_sum$tag)
#tag_sum <- tag_sum[tag_sum$sum > 39, ]
tag_sum <- tag_sum[(dim(tag_sum)[1]-19):dim(tag_sum)[1] ,]

# Number of most popular tag
ggplot(tag_sum, aes(x = tag, y = sum, fill = tag)) + 
  geom_bar(stat = "identity", colour = "black") + 
  coord_flip() +
  labs(title = "Most popular tags", x = "", y = "") + 
  geom_text(aes(label = sum), hjust = -0.2, vjust = 0.4) + 
  theme_few() +
  theme(legend.position = "None") +
  theme(axis.text.x=element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank()) 




### Create an appropriate dataframe with user, bookmark for each tag ####
bookmark_and_tag <- cbind(user = BG0$userID, bookmark = BG0$bookmarkID, bookmark_and_tag, stringsAsFactors = FALSE)
bookmark_and_tag <- melt(bookmark_and_tag, id = c("user", "bookmark"))
bookmark_and_tag$variable<- gsub("[.]", " ", bookmark_and_tag$variable)
bookmark_and_tag <- bookmark_and_tag[bookmark_and_tag$value == 1, ] 
bookmark_and_tag$value <- NULL
colnames(bookmark_and_tag) <- c("user", "bookmark","tag")
bookmark_and_tag$tag <- factor(bookmark_and_tag$tag, levels = tag_sum$tag)
bookmark_and_tag <- bookmark_and_tag[complete.cases(bookmark_and_tag), ]


# Scatter plots of tag based on user and bookmark
ggplot(bookmark_and_tag, aes(x = user, y = bookmark, colour = tag)) + 
  geom_jitter(alpha = 0.2) +
  scale_y_continuous(breaks = c(1000,2000,3000,4000,5000), 
                     limits = c(1, 5001)) +
  scale_x_continuous(breaks = c(1,20000,40000,60000,80000,100000), 
                     labels = c("0","20k", "40k", "60k", "80k", "100k"), 
                     limits = c(1, 110000)) +
  labs(title = "bookmark and user",  y = "bookmarkID",x = "userID") +
  facet_wrap(~ tag, nrow = 4) +
  theme_bw() + 
  theme(legend.position = "None") +
  annotation_logticks(sides = "lr", colour = "gray") 




### Scatter plot of mean user and mean bookmark of most popular tag####

bookmark_and_tag_stats <- as.data.frame(by(bookmark_and_tag$user, bookmark_and_tag$tag, mean))
bookmark_and_tag_stats$mean_gross <- as.data.frame(by(bookmark_and_tag$bookmark, bookmark_and_tag$tag, mean))$value
colnames(bookmark_and_tag_stats) <- c("tag", "mean_user", "mean_bookmark")

ggplot(bookmark_and_tag_stats, aes(x = mean_user, y = mean_bookmark)) + 
  geom_point(alpha = 1, colour = "black", shape = 21, size = 2, fill = "darkgreen") +
  scale_y_continuous(breaks = c(2000,2500,3000,3500), 
                     #labels = c("30", "40", "50", "60", "70"), 
                     limits = c(1900,3500)) +
  scale_x_continuous(breaks = c(1000,2500,4000,5500), 
                     #labels = c("2k", "4k","6k","8k","10k"),
                     limits = c(1000,6500)) +
  labs(title = "Mean bookmark and mean user", y = "bookmark", x = "user ID") +
  geom_text(aes(label = tag), hjust = 0, nudge_x = 0.015) +
  theme_bw() + 
  theme(legend.position = "None")








