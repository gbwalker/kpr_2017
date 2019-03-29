#Replication materials for summary statistics in paper: 

#set your wd where the replication file is here.
wdir <- ""
setwd(wdir)
posts <- read.csv("ReplicationFile/PostDataSets/posts_all.csv", header=T, sep = ",",encoding="UTF-8")
nrow(posts)
#43,757 posts extracted 

length(table(posts$What.is.the.name.of.the.organization.making.posts.))
  # 248 different government bureaus

org.unknown <- table(tolower(posts$What.is.the.name.of.the.organization.making.posts.) == "unknown")[2]
org.na <- table(tolower(posts$What.is.the.name.of.the.organization.making.posts.) == "n/a")[2]
(dim(posts)[1] - org.unknown - org.na) / dim(posts)[1]
  # 99.4% known
org.unknown + org.na
  #281 can't identify

sort(table(posts$What.is.the.name.of.the.organization.making.posts.), decreasing=T)[1:5]
sort(table(posts$What.is.the.name.of.the.organization.making.posts.), decreasing=T)[1:5]/dim(posts)[1]
  # 9,159 posts (20.9% of the 43,757 total) made directly by the Zhanggong Internet Propaganda Office
  # 1,672 by Shuixi Township (水西镇, one of several townships in Zhanggong)
  # 1,620 by Nanwai sub-district (南外街道 (one of several sub-districts in Zhanggong)

#install.packages("stringr")
library(stringr)
commerce <- which(!is.na(str_locate(names(table(posts$What.is.the.name.of.the.organization.making.posts.)), "商务")))[1:3]
sum(table(posts$What.is.the.name.of.the.organization.making.posts.)[commerce])
sum(table(posts$What.is.the.name.of.the.organization.making.posts.)[commerce])/dim(posts)[1]
  # 2,343 (5.4%) by the Zhanggong district Bureau of Commerce (区商务局)
  
sites <- read.csv("ReplicationFile/PostDataSets/postsites.csv", sep="\t", header=T)
  # condensed list of sites, done by RAs
posts2 <- merge(posts, sites, all.x=T, by.x="site", by.y="Site")

posts2$Label[is.na(posts2$Label)] <- "UNKNOWN"
prop <- as.numeric(sort(prop.table(table(posts2$Label)), decreasing =T))
sites.info <- cbind.data.frame(prop,names(sort(prop.table(table(posts2$Label)), decreasing =T)))
sites.info
  # 29.98% did not contain a URL
  
colnames(sites.info)[2] <- "site"
sites.info$type <- c("unknown","forum","commercial","commercial", "commercial", 
                     "news", "forum", "forum","commercial", "forum", "news", "news", 
                     "commercial", "news", "commercial", "news","news", "commercial","commercial",
                     "forum","commercial","news")
sites.info$prop.nona <- sites.info$prop / sum(sites.info$prop[2:22])
sites.info$prop.nona[1] <- 0

sum(sites.info$prop.nona[sites.info$type == "forum"]) + 
  sum(sites.info$prop.nona[sites.info$type == "news"])
  # 53.38% of the 50c posts were comments on government sites

sum(sites.info$prop.nona[sites.info$type == "commercial"])
  # 46.62% were on commerical sites
  
sites.info$prop.nona[sites.info$site=="Sina_weibo"] / sum(sites.info$prop.nona[sites.info$type == "commercial"])
  # 53.98% were on Sina Weibo

sites.info$prop.nona[sites.info$site=="Tencent_weibo"] / sum(sites.info$prop.nona[sites.info$type == "commercial"])
  # 32.10% were on Tencent WEibo

sites.info$prop.nona[sites.info$site=="Baidu _tieba"] / sum(sites.info$prop.nona[sites.info$type == "commercial"])
  # 10.75% on Baidu Tieba

sites.info$prop.nona[sites.info$site=="Tencent_qzone"] / sum(sites.info$prop.nona[sites.info$type == "commercial"])
  # 2.69% on Tencent QZone

#Some of posts the RAs couldn't find content for, 
#these are marked as UNKNOWN
table(posts$content == "UNKNOWN")

#Summary statistics for posts from leaked 50c accounts
setwd(wdir)
data <- read.csv("ReplicationFile/PostDataSets/knownWeibos_zg.csv")

#how many posts 
nrow(data)

#total accounts known to be 50c accounts
length(unique(data$account_id))

#total exclusive vs ordinary
byaccount <- tapply(data$exclusive, data$account_id, unique)
table(byaccount)

#created month
createdbyaccount <- tapply(as.character(data$datecreated), data$account_id, function (x) unique(x))
monthcreated <- format(as.Date(createdbyaccount), "%Y-%m")  

#mean creations per month
mean(table(monthcreated))

#lots created during Xi's meeting in Feburary 2014
sum(monthcreated=="2014-02")
sum(monthcreated=="2014-03")
