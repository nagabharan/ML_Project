if (!require("rminer")) 
{
    install.packages("rminer", repos="http://cran.rstudio.com/") 
    library("rminer")
}

# read previously saved file
por=read.table(file="dataset/por2.csv",header=TRUE)

# select inputs:
inputs=2:29 # select from 2 ("sex") to 29 ("health")

# select outputs: regression task
g3=which(names(por)=="G3")

cat("output class:",class(por[,g3]),"\n")

# fit holdout example:
H=holdout(por$G3,ratio=3/4,seed=12345)

R1=fit(G3~.,por[H$tr,c(inputs,g3)],model="randomForest")
P1=predict(R1,por[H$ts,c(inputs,g3)])
target1=por[H$ts,]$G3
e1=mmetric(target1,P1,metric=c("MAE","R22"))
error=paste("RF, holdout: MAE=",round(e1[1],2),", R2=",round(e1[2],2),sep="")

pdf("graphs/por-reg-rf-holdout.pdf")
mgraph(target1,P1,graph="RSC",Grid=10,main=error)
dev.off()

# rpart example with k-fold cross-validation
R2=crossvaldata(G3~.,por[,c(inputs,g3)],fit,predict,ngroup=10,seed=123,model="randomForest",task="reg")
P2=R2$cv.fit # k-fold predictions on full dataset
e2=mmetric(por$G3,P2,metric=c("MAE","R22"))
error2=paste("RF, 10-fold: MAE=",round(e2[1],2),", R2=",round(e2[2],2),sep="")

pdf("graphs/por-reg-rf-10fold.pdf")
mgraph(por$G3,P2,graph="RSC",Grid=10,main=error2)
dev.off()
