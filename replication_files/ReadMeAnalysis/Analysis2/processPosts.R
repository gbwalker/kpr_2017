#This file processes the posts scraped from leaked Weibo accounts for the ReadMe Analysis
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
all_dataseg <- read.csv("ReplicationFile/PostDataSets/knownWeibos_zg.csv")

#We sampled 1 post from each account for the training set.
#Recode to be consistent with Analysis 1
table(all_dataseg$Category)
all_dataseg$category <- NA
all_dataseg$category[all_dataseg$Category=="Argumentative praise or criticism"] <- 6
all_dataseg$category[all_dataseg$Category=="Factual Reporting"] <- 5
all_dataseg$category[all_dataseg$Category=="Cheerleading for China"] <- 4
all_dataseg$category[all_dataseg$Category=="Non-argumentative Praise or Suggestions"] <- 3
all_dataseg$category[all_dataseg$Category=="Other"] <- 2

all_dataseg$training <- ifelse(is.na(all_dataseg$category),0,1)

#So ReadMe can run, take a stratified sample of posts from the test dataset by account
test_data <- all_dataseg[all_dataseg$training==0,]

#sample like we did for the training data by account
acct <- split(test_data, list(test_data$account_id))

set.seed(1234)
post.samp <- lapply(acct, function(x) x[sample(1:nrow(x), min(nrow(x),25), FALSE),])  # randomly select 25 rows from each list
out <- do.call(rbind, post.samp) # combine, basically have 25 post or less from each account

new_dataseg <- rbind(out, all_dataseg[all_dataseg$training==1,])

all_data <- data.frame(ROWID = 1:nrow(new_dataseg), 
                       TRUTH = new_dataseg$category,
                       TRAININGSET = new_dataseg$training, 
                       post_content = new_dataseg$textseg)

#create a folder called raw_text_files.
#you will write all individual files to this folder to do the ReadMes
setwd("raw_text_files")

file.remove(list.files())
ddply(all_data,~ROWID, function(post){
    post <- data.frame(post)
    write(as.character(post$post_content), paste0(as.character(post$ROWID), ".txt"))
})

processed_files <- paste0(seq(1,nrow(all_data)),".txt")
control_file_data <- data.frame("ROWID" = processed_files,
                                "TRUTH" = all_data$TRUTH,
                                "TRAININGSET" = all_data$TRAININGSET)

#Creates the control file
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
          file="ReplicationFile/ReadMeAnalysis/Analysis2/ReadMeBootResults.csv")

##########################
#Exclusive only analysis#
############################

exclusive <- new_dataseg[new_dataseg$exclusive==1,]
all_data <- data.frame(ROWID = 1:nrow(exclusive), 
                       TRUTH = exclusive$category,
                       TRAININGSET = exclusive$training, 
                       post_content = exclusive$textseg)

#create a folder called raw_text_files.
#you will write all individual files to this folder to do the ReadMes
setwd("raw_text_files")
file.remove(list.files())
ddply(all_data,~ROWID, function(post){
  post <- data.frame(post)
  write(as.character(post$post_content), paste0(as.character(post$ROWID), ".txt"))
})

processed_files <- paste0(seq(1,nrow(all_data)),".txt")
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

ests <- lapply(boot, function (x) x$est.CSMF)
ests2 <- matrix(0, nrow=50, ncol=6)
colnames(ests2) <- seq(1,6)
for(i in 1:length(ests)){
  ests2[i,colnames(ests2)%in%names(ests[[i]])] <- ests[[i]]
}

setwd(wdir)
write.csv(ests2, row.names=F,
          file="ReplicationFile/ReadMeAnalysis/Analysis2/ReadMeBootResults_Exclusive.csv")


##########################
#Ordinary analysis#
############################

ordinary <- new_dataseg[new_dataseg$exclusive==0,]
all_data <- data.frame(ROWID = 1:nrow(ordinary), 
                       TRUTH = ordinary$category,
                       TRAININGSET = ordinary$training, 
                       post_content = ordinary$textseg)

#create a folder called raw_text_files.
#you will write all individual files to this folder to do the ReadMes
setwd("raw_text_files")
file.remove(list.files())
ddply(all_data,~ROWID, function(post){
  post <- data.frame(post)
  write(as.character(post$post_content), paste0(as.character(post$ROWID), ".txt"))
})

processed_files <- paste0(seq(1,nrow(all_data)),".txt")
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

ests <- lapply(boot, function (x) x$est.CSMF)
ests2 <- matrix(0, nrow=50, ncol=6)
colnames(ests2) <- seq(1,6)
for(i in 1:length(ests)){
  ests2[i,colnames(ests2)%in%names(ests[[i]])] <- ests[[i]]
}

setwd(wdir)
write.csv(ests2, row.names=F,
          file="ReplicationFile/ReadMeAnalysis/Analysis2/ReadMeBootResults_Ordinary.csv")



