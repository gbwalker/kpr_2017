##Original leaked file available in Web archive, see notes.
##Original leaked file needed to run commented code.

#Combine all the data
#setwd("CsvsOut")
#files <- list.files()
#i <- 1
#all <- read.csv(files[i], quote="\"")
#all$folder <- files[i]

#for(i in 2:length(files)){
#  alltemp <- read.csv(files[i], quote="\"")
#  alltemp$folder <- files[i]
#  all <- rbind(all, alltemp)
#}

#Number of e-mails in network dataset
#dim(all)

#Take out auto-replies 
#library(stringr)
#auto <- sapply(all$subject, function(x) str_detect(x, "自动回复"))
#all <- all[-auto,]

#put all tos together
#alltos <- NULL
#library(stringr)
#for(i in 1:nrow(all)){
#  split <- str_split(all$to[i], "'")[[1]]
#  split <- split[!split%in%c("[", ", ", "]")]
#  alltos <- c(alltos, split)
#}

#Combine all people in dataset into a network matrix
#tos <- unique(alltos)
#froms <- unique(as.character(all$from))
#everyone <- unique(c(tos,froms))
#network <- matrix(0,nrow=length(everyone), ncol=length(everyone))
#rownames(network) <- everyone
#colnames(network) <- everyone

#Fill network matrix
#for(i in 1:nrow(all)){
#  split <- str_split(all$to[i], "'")[[1]]
#  split <- split[!split%in%c("[", ", ", "]")]
#  for(j in 1:length(split)){
#    network[rownames(network)==all$from[i], colnames(network)%in%split] <-
#      network[rownames(network)==all$from[i], colnames(network)%in%split] +1
#  }
#}

##Anonymize and write out network
#rownames(network) <- 1:nrow(network)
#colnames(network) <- 1:ncol(network)
#write.csv(network, "Network.csv", row.names=F)

#Create a graph of the network using igraph
network <- as.matrix(read.csv("Network.csv", header=T))
row.names(network) <- network[,1]
network <- network[,-1]
library(igraph)
diag(network) <- 0
jpeg("50CentNetwork.jpeg", res=1000, width=8, height=7,
     units="in")
set.seed(3487)
g <- graph.adjacency(network, mode="undirected")
g <- simplify(g)

l <-layout.fruchterman.reingold(g) 
l[,2] <- l[,2]*-1

V(g)$label <- ""
V(g)$size <- 2
V(g)$label.cex <- .1

plot(g, layout=l)
text(.55,.1,"Commentators Reporting to \n Zhanggong Internet \n Propaganda Office")
text(x =.25, y = -.05, '{', srt = 45, cex = 2, family = 'Helvetica Neue UltraLight')
arrows(-.55,-.55,-.1,-.45, lwd=3)
text(-.9,-.56,"Zhanggong Internet \n  Propaganda Office")
text(0,.95,"Higher Level Offices \n Reported To")
text(x = 0, y = 0.8, '{', srt = 90, cex = 2, family = 'Helvetica Neue UltraLight')
dev.off()
