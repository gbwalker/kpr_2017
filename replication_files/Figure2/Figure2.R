#Replication file for Figure 2

#Set your wd here.
wdir <- ""
setwd(wdir)
posts <- read.csv("ReplicationFile/PostDataSets/posts_all.csv", header=T, encoding="UTF-8")

#Number of posts
nrow(posts)

#Change into a date
posts$PostDate2 <- as.Date(posts$PostDate)
postcount <- table(posts$PostDate2)

#Sequence of dates
dateseq <- seq(min(posts$PostDate2), max(posts$PostDate2), by="day")
dateseq <- dateseq[!dateseq%in%as.Date(names(postcount))]
zeros <- rep(0, length(dateseq))
names(zeros) <- dateseq
postcount <- c(postcount, zeros)
postcount <- postcount[order(as.Date(names(postcount)))]

#for x-axis
library(lubridate)
months <- seq(as.Date("2013-01-01"), as.Date("2014-12-01"), by="month")
monthslab <- months(months, abbreviate=T)
#Plot time plot
pdf("bursts.pdf", width=10)
plot(as.Date(names(postcount)),as.vector(postcount), type="l", ylab="Count of Posts",xlab="Date (Jan 2013 - Dec 2014)",xaxt="n",ylim=c(0,4000))
axis(1,at=months,labels=monthslab)
text(as.Date("2013-04-07"),3600,"1. Qingming\nfestival\n(April)", col="red")
text(as.Date("2013-05-23"),1400,"2. China\nDream\n(May)", col="red")
text(as.Date("2013-07-01"),2000,"3. Shanshan\nriots (July)", col="red")
arrows(as.Date("2013-07-01"), 1800, as.Date("2013-07-01"), 1050, col="red", length=0.1)
text(as.Date("2013-11-09"), 600, "4. 3rd plenum\nCCP 18th\nCongress (Nov)", col="red")
text(as.Date("2014-02-12"), 1150, "5. Two meetings\n(Feb)", col="red")
arrows(as.Date("2014-02-12"), 950, as.Date("2014-02-12"), 600, col="red", length=0.1)
text(as.Date("2014-05-09"),2200,"6. Urumqi rail\nexplosion (May)", col="red")
text(as.Date("2014-07-15"),1300,"7. Gov't\nforum,\npraise\ncentral\nsubsidy\n(Jul-Aug)", col="red")
segments(as.Date("2014-07-15"),750,as.Date("2014-07-15"),650, col="red")
segments(as.Date("2014-06-15"),650,as.Date("2014-08-30"), col="red")
segments(as.Date("2014-06-15"),650,as.Date("2014-06-15"),600, col="red")
segments(as.Date("2014-08-30"),650,as.Date("2014-08-30"),600, col="red")
text(as.Date("2014-10-08"),3000,"8. Martyr's\nDay\n(Oct)", col="red", pos=2)
dev.off()