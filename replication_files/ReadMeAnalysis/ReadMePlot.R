#All together now: ReadMe
#Set your home directory to replication file 
#1: Taunting of foreign countries
#2: Other
#3: Non-argumentative praise/criticism
#4: Cheerleading
#5: Factual reporting
#6: Argumentative praise/criticism

homedir <- ""
setwd(homedir)
setwd("ReadMeAnalysis/Analysis1")
results1 <- read.csv("ReadMeBootResults.csv")
setwd(homedir)
setwd("ReadMeAnalysis/Analysis2")
results2 <- read.csv("ReadMeBootResults.csv")
selectresults <- read.csv("ReadMeBootResults_Exclusive.csv")
notselectresults <- read.csv("ReadMeBootResults_Ordinary.csv")
setwd(homedir)
setwd("ReadMeAnalysis/Analysis3")
results3 <- read.csv("ReadMeBootResults.csv")
setwd(homedir)
setwd("ReadMeAnalysis/Analysis4")
results4 <- read.csv("ReadMeBootResults.csv")

#Normalize to remove Other category (Category 2)
results1 <- results1/(1-results1[,2])
results2 <- results2/(1-results2[,2])
results3 <- results3/(1-results3[,2])
results4 <- results4/(1-results4[,2])
selectresults <- selectresults/(1-selectresults[,2])
notselectresults <- notselectresults/(1-notselectresults[,2])

#Point estimates
#Analysis 1
apply(results1[,c(1,3:6)],2,mean)
#Analysis 2
apply(results2[,c(1,3:6)],2,mean)
#Ordinary
apply(notselectresults[,c(1,3:6)],2,mean)
#Exclusive
apply(selectresults[,c(1,3:6)],2,mean)
#Analysis 3
apply(results3[,c(1,3:6)],2,mean)
#Analysis 4
apply(results4[,c(1,3:6)],2,mean)

pdf("AllTogether.pdf", width=9.5, height=7)
plot(c(1,3,5,4,2), apply(results1[,c(1,3:6)],2,mean), xaxt="n", ylim=c(0,1), xlab="",
     ylab="Proportion", pch=16, xlim=c(0.5,6.1))
segments(c(1,3,5,4,2), apply(results1[,c(1,3:6)],2, function (x)
    quantile(x,.975)), c(1,3,5,4,2),apply(results1[,c(1,3:6)],2, function (x)
      quantile(x,.025)))

points(c(1,3,5,4,2)+.1, apply(results2[,c(1,3:6)],2,mean), pch=17)
segments(c(1,3,5,4,2) +.1, apply(results2[,c(1,3:6)],2, function (x)
  quantile(x,.975)), c(1,3,5,4,2)+.1,apply(results2[,c(1,3:6)],2, function (x)
    quantile(x,.025)), lty=2)

points(c(1,3,5,4,2)+.2, apply(selectresults[,c(1,3:6)],2,mean), pch=7, col="red")
segments(c(1,3,5,4,2) +.2, apply(selectresults[,c(1,3:6)],2, function (x)
  quantile(x,.975)), c(1,3,5,4,2)+.2,apply(selectresults[,c(1,3:6)],2, function (x)
    quantile(x,.025)), lty=2, col="red")

points(c(1,3,5,4,2)+.3, apply(notselectresults[,c(1,3:6)],2,mean), pch=8, col="red")
segments(c(1,3,5,4,2) +.3, apply(notselectresults[,c(1,3:6)],2, function (x)
  quantile(x,.975)), c(1,3,5,4,2)+.3,apply(notselectresults[,c(1,3:6)],2, function (x)
    quantile(x,.025)), lty=2, col="red")

points(c(1,3,5,4,2)+.4, apply(results3[,c(1,3:6)],2,mean), pch=4)
segments(c(1,3,5,4,2) +.4, apply(results3[,c(1,3:6)],2, function (x)
  quantile(x,.975)), c(1,3,5,4,2)+.4,apply(results3[,c(1,3:6)],2, function (x)
    quantile(x,.025)), lty=3)

points(c(1,3,5,4,2)+.5, apply(results4[,c(1,3:6)],2,mean), pch=5)
segments(c(1,3,5,4,2) +.5, apply(results4[,c(1,3:6)],2, function (x)
  quantile(x,.975)), c(1,3,5,4,2)+.5,apply(results4[,c(1,3:6)],2, function (x)
    quantile(x,.025)), lty=5)

legend(.4,1, c("Leaked e-mails, all sites", "Leaked accounts, Weibo",
                "Leaked accounts, exclusive",
                "Leaked accounts, ordinary", "Within county prediction, all posts",  "Out of county prediction"),
       lty=c(1,2,2,2,3,4), pch=c(16,17,7,8,4,5), col=c(rep("black",2),
                                                       rep("red",2), rep("black",2)))

text(1.8, .17, "Argumentative Praise \n or Criticism")
text(.9, 0.08, "Taunting of Foreign \n Countries")
text(5.5, .8, "Cheerleading")
text(4.8, .17, "Factual \n Reporting")
text(3.2, 0, "Non-argumentative \n Praise or Suggestions")

dev.off()

