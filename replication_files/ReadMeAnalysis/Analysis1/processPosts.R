#This file processes the known fifty cent posts for the ReadMe Analysis
#and computes read me.
library(plyr)
library(stringr)
library(gdata)
library(tm)
library(ReadMe)

#set working directory to where replication file folder is
wdir <- ""
setwd(wdir)
#Segmented File in ReadMe format
all_dataseg <- read.csv("ReplicationFile/PostDataSets/posts_all.csv")

#We sampled 200 posts, 188 had codeable content
table(all_dataseg$category)
# Factual Reporting --> 5
# Cheerleading for China -> 4
# Non-argumentative Praise or Suggestions -> 3
# Other -> 2
# Taunting of Foreign Countries -> 1

all_dataseg$training <- ifelse(is.na(all_dataseg$category),0,1)
#creates new dataset in the format that ReadMe requires
all_data <- data.frame(ROWID = 1:nrow(all_dataseg), 
                              TRUTH = all_dataseg$category,
                              TRAININGSET = all_dataseg$training, 
                              post_content = all_dataseg$textseg)

#create a folder called raw_text_files.
#you will write all individual files to this folder to do the ReadMes
setwd("raw_text_files")
file.remove(list.files())
#writes files to folder
ddply(all_data,~ROWID, function(post){
    post <- data.frame(post)
    write(as.character(post$post_content), paste0(as.character(post$ROWID), ".txt"))
})

#Creates the control file
processed_files <- paste0(seq(1,nrow(all_dataseg)),".txt")
control_file_data <- data.frame("ROWID" = processed_files,
                                "TRUTH" = all_data$TRUTH,
                                "TRAININGSET" = all_data$TRAININGSET)

write.table(control_file_data,"raw_text_files/control.txt", row.names=F,
            sep=",", quote=FALSE)

undergrad.results <- undergrad(sep=",", stem=F, alphanumeric.only=F, threshold=.001)

undergrad.preprocess <- preprocess(undergrad.results)

readme.results <- readme(undergrad.preprocess)

#Bootstrap
boot <- list()
for(i in c(1:50)){
  databoot <- control_file_data
  databoot <- databoot[sample(1:nrow(databoot),
                              nrow(databoot),
                              replace=T),]
  write.table(databoot, "control.txt", row.names=F,
              sep=",",
              quote=FALSE)
  undergrad.results <- undergrad(sep=",", stem=F,
                                 alphanumeric.only=F,
                                 threshold=.001)
  undergrad.preprocess <-
    preprocess(undergrad.results)
  
  boot[[i]] <- tryCatch(readme(undergrad.preprocess),
                        error=function
                        (e)
                          print(e))
  print(i)
}

#Formats output
ests <- lapply(boot, function (x) x$est.CSMF)
ests2 <- matrix(0, nrow=50, ncol=6)
colnames(ests2) <- seq(1,6)
for(i in 1:length(ests)){
  ests2[i,colnames(ests2)%in%names(ests[[i]])] <- ests[[i]]
}

setwd(wdir)
write.csv(ests2, row.names=F,
          file="ReplicationFile/ReadMeAnalysis/Analysis1/ReadMeBootResults.csv")




