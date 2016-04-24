if (!require("rminer")) 
{
  install.packages("rminer", repos="http://cran.rstudio.com/") 
  library("rminer")
}

# read previously saved file
math=read.table(file="math2.csv",header=TRUE)

# select inputs:
inputs=2:29 # select from 2 ("sex") to 29 ("health")

# select outputs: regression task
g3=which(names(math)=="G3")

cat("output class:",class(math[,g3]),"\n")

# fit holdout example:
H=holdout(math$G3,ratio=3/4,seed=12345)

R1=fit(G3~.,math[H$tr,c(inputs,g3)],model="randomForest")
P1=predict(R1,math[H$ts,c(inputs,g3)])
target1=math[H$ts,]$G3
e1=mmetric(target1,P1,metric=c("MAE","R22"))
error=paste("RF, holdout: MAE=",round(e1[1],2),", R2=",round(e1[2],2),sep="")

pdf("graphs/math-reg-rf-holdout.pdf")
mgraph(target1,P1,graph="RSC",Grid=10,main=error)
dev.off()

# rpart example with k-fold cross-validation
R2=crossvaldata(G3~.,math[,c(inputs,g3)],fit,predict,ngroup=10,seed=123,model="randomForest",task="reg")
P2=R2$cv.fit # k-fold predictions on full dataset
e2=mmetric(math$G3,P2,metric=c("MAE","R22"))
error2=paste("RF, 10-fold: MAE=",round(e2[1],2),", R2=",round(e2[2],2),sep="")

pdf("graphs/math-reg-rf-10fold.pdf")
mgraph(math$G3,P2,graph="RSC",Grid=10,main=error2)
dev.off()