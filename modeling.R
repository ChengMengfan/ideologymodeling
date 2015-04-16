###########################################################################################
# merge the sunlight data with the ideology score #

library(foreign)
capitolwords <- read.csv("cleanedwords.csv", header = TRUE)
senate <- read.dta("SL01113D21_PRES_12.DTA")
house <- read.dta("HL01113D21_PRES_12.DTA")

# process senate and house data #
senate <- senate[!(senate$cong!=113),]
house <- house[!(house$cong!=113),]

bridge <- read.csv("results_plus_hand_entry.csv", header = TRUE)

congress <- rbind(senate, house)
merged1 <- merge(congress, bridge, by="idno")
merged2 <- merge(merged1, capitolwords, by="bioguide_id")

write.csv(merged2, "merged.csv")

############################################################################################
# getting the bigrams and trigrams #
Sys.setenv(JAVA_HOME = '/Library/Java//Home')
Sys.setenv(LD_LIBRARY_PATH = '$LD_LIBRARY_PATH:$JAVA_HOME/lib')
## install.packages('rJava', type='source')
library(rJava)
options(java.parameters = "-Xmx8000m")
library(RWeka)

merged <- read.csv("merged.csv",header=TRUE)
speaking <- merged$speaking

republicans <- subset(merged, speaker_party=='R')
democrats <- subset(merged, speaker_party=='D')
r_speaking <- republicans$speaking
d_speaking <- democrats$speaking

bigramsR_unsorted <- NGramTokenizer(r_speaking, Weka_control(min = 2, max = 2))
trigramsR_unsorted <- NGramTokenizer(r_speaking, Weka_control(min = 3, max = 3))

bigramsD_unsorted <- NGramTokenizer(d_speaking, Weka_control(min = 2, max = 2))
trigramsD_unsorted <- NGramTokenizer(d_speaking, Weka_control(min = 3, max = 3))

bigramsR <- sort(table(bigramsR_unsorted),decreasing=T)

trigramsR <- sort(table(trigramsR_unsorted),decreasing=T)

bigramsD <- sort(table(bigramsD_unsorted),decreasing=T)

trigramsD <- sort(table(trigramsD_unsorted),decreasing=T)

# Top bigrams and trigrams of Republicans and Democrats with the highest frequency #

topbigramsR <- head(bigramsR,100)
toptrigramsR <- head(trigramsR,100)
topbigramsD <- head(bigramsD,100)
toptrigramsD <- head(trigramsD,100)

write.csv(topbigramsR, "topbigramsR.csv")
write.csv(topbigramsD, "topbigramsD.csv")
write.csv(toptrigramsR, "toptrigramsR.csv")
write.csv(toptrigramsD, "toptrigramsD.csv")

# getting the chi-square for the bigrams and trigrams #
# merge the Republicans and the Democrats #

colnames(bigramsR) <- c("bigram","Rscore")
colnames(bigramsD) <- c("bigram","Dscore")
colnames(trigramsR) <- c("trigram","Rscore")
colnames(trigramsD) <- c("trigram","Dscore")

totalbi <- merge(bigramsR,bigramsD, by="bigram")
totaltri <- merge(trigramsR,trigramsD,by="trigram")

totalbi <- read.csv("totalbi.csv", header = TRUE)
totaltri <- read.csv("totaltri.csv", header = TRUE)

# calculating chi2 for each bigram and trigram #

bigrams <- NGramTokenizer(speaking, Weka_control(min = 2, max = 2))
trigrams <- NGramTokenizer(speaking, Weka_control(min = 3, max = 3)) 

bifr <- totalbi[,4]
bifd <- totalbi[,3]
bifrNOT <- as.numeric(length(bigrams))-bifr
bifdNOT <- as.numeric(length(bigrams))-bifd

chisq <- function() {
  numerator = (bifr*bifdNOT-bifd*bifrNOT)^2
  denominator = (bifr+bifd)*(bifr+bifrNOT)*(bifd+bifdNOT)*(bifrNOT+bifdNOT)
  return (numerator/denominator)
}

trifr <- totaltri[,4]
trifd <- totaltri[,3]
trifrNOT <- as.numeric(length(trigrams))-trifr
trifdNOT <- as.numeric(length(trigrams))-trifd

chisqtri <- function() {
  numerator = (trifr*trifdNOT-trifd*trifrNOT)^2
  denominator = (trifr+trifd)*(trifr+trifrNOT)*(trifd+bifdNOT)*(trifrNOT+trifdNOT)
  return (numerator/denominator)
}

bigramchisq <- cbind(totalbi,chisq())
bigramchisq <- bigramchisq[order(chisq(),decreasing=TRUE),]

trigramchisq <- cbind(totaltri,chisqtri())
trigramchisq <- trigramchisq[order(chisqtri(),decreasing=TRUE),]

# keep the top 500 with the highest chi-square #
topbigram <- head(bigramchisq,500)
toptrigram <- head(trigramchisq,500)

write.csv(topbigram,"topbigrams.csv")
write.csv(toptrigram,"toptrigrams.csv")

##############################################################################################
# building the model #
idscore <- merged[,10]
name_party <- merged[,19]
modeldata <- cbind(idscore,name_party)

# create new columns for bigrams and name the columns #
zeromatrix <- matrix(0,nrow=nrow(modeldata),ncol=500)
modeldata <- cbind(modeldata,zeromatrix)
topbigramname <- t(matrix(topbigram[,2]))


for (i in 1:500) {
  if (i==1) { names <- c(toString(topbigramname[i])) }
  else { names <- c(names,toString(topbigramname[i])) }
}

colnames(modeldata) <- c("IdeologyScore","Name_Party",names)

# check whether the speaker has said those bigrams ＃

for (i in 1:nrow(modeldata)) {
  for (j in 1:500) { 
    if (grepl(names[j],toString(speaking[i]),ignore.case = TRUE, perl = FALSE,fixed = FALSE, useBytes = FALSE) == TRUE) 
      
    { modeldata[i,j] <- 1}
    else 
    { modeldata[i,j] <- 0 }
  }
  
}

# create the Ridge model #

library(glmnet)
library(ISLR)

grid=10^seq(10,-2,length=100)

modeldata <- data.frame(modeldata)

x=model.matrix(IdeologyScore~.,data=modeldata)[,-1]
y=modeldata$IdeologyScore

ridge.mod=glmnet(x,y,alpha=0,lambda=grid)

dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

predict(ridge.mod,s=50,type="coefficients")[1:20,]

# split samples into a training set and a test set #
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

# fit a ridge regression model on the training set, and evaluate its MSE on the test set, using λ = 4 #
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
# -> test MSE #

# check large lambda and lambda=0 #
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)
lm(y~x,subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:20,]

# cross-validation using cv.glmnet() #
set.seed(1)
cv.out=cv.glmnet(x[train ,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
# bestlambda=0.04307892 #

# therefore #
ridge.pred=predict(ridge.mod,s=bestlam ,newx=x[test,])
mean((ridge.pred-y.test)^2)

# refit our ridge regression model on the full data set #
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]
