setwd("/Users/Mengfan")
capitolwords <- read.csv("sample.csv",header=TRUE)

library(glmnet)
grid=10^seq(10,-2,length=100)

x=model.matrix(dwnom1~.,data=capitolwords)[,-1]
y=capitolwords$dwnom1

ridge.mod=glmnet(x,y,alpha=0,lambda=grid)

dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

predict(ridge.mod,s=50,type="coefficients")[1:20,]

set.seed(1)