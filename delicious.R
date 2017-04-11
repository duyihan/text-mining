library(igraph)
library(datasets)

library(ggplot2)
library(ggthemes)
library(reshape2)
library(taRifx)

setwd("~/EISTI/Analyse-des-reseaux-sociaux/projet/delicious")

bookmark                         <- read.delim("bookmarks.dat", header = TRUE, sep="\t")
bookmark_tag                     <- read.delim("bookmark_tags.dat", header = TRUE, sep="\t")
tags                             <- read.delim("tags.dat", header = TRUE, sep="\t")
#user_contacts_timestamps         <- read.delim("user_contacts_timestamps.dat", header = TRUE, sep="\t")
user_contacts                    <- read.delim("user_contacts.dat", header = TRUE, sep="\t")
#user_taggedbookmarks_timestamps  <- read.delim("user_taggedbookmarks_timestamps.dat", header = TRUE, sep="\t")
user_taggedbookmarks             <- read.delim("user_taggedbookmarks.dat", header = TRUE, sep="\t")



### create a net ####
#net <- graph_from_data_frame(d=user_contacts,directed = T)
#V(net)
#plot( net, vertex.size=0, vertex.label=NA,edge.arrow.size=0)


### create a new data ####
#delete bookmark > 5000
BG<-user_taggedbookmarks[user_taggedbookmarks$bookmarkID<501,]
bookmark_tag<-bookmark_tag[bookmark_tag$bookmarkID<501,]
bookmark<-bookmark[bookmark$id<501,]
#BG<-user_taggedbookmarks[user_taggedbookmarks$bookmarkID<51,]

#remove ???
BG <- BG[,c("userID","bookmarkID","tagID")]

#tri BG
#BG <- BG [with(BG , order(decreasing = TRUE,-bookmarkID,tagWeight)), ]


### add value of tags -----BG ***

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
  scale_y_continuous(breaks = c(100,200,300,400,500), 
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



