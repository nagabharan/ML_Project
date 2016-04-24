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

pdf("graphs/por-reg-scatterplots.pdf")
par(mfrow=c(2,2))
sink("Por-Regression-Output.txt")

# holdout
R1=fit(G3~.,por[H$tr,c(inputs,g3)],model="rpart")
P1=predict(R1,por[H$ts,c(inputs,g3)])
target1=por[H$ts,]$G3
e1=mmetric(target1,P1,metric=c("RMSE","MAE"))
error=paste("DT, holdout: RMSE=",round(e1[1],2),", R2=",round(e1[2],2),sep="")
print(error)
mgraph(target1,P1,graph="RSC",Grid=10,main=error)

# k-fold cross-validation
R2=crossvaldata(G3~.,por[,c(inputs,g3)],fit,predict,ngroup=10,seed=123,model="rpart",task="reg")
P2=R2$cv.fit # k-fold predictions on full dataset
e2=mmetric(por$G3,P2,metric=c("RMSE","MAE"))
error2=paste("DT, 10-fold: RMSE=",round(e2[1],2),", MAE=",round(e2[2],2),sep="")
print(error2)
mgraph(por$G3,P2,graph="RSC",Grid=10,main=error2)

# holdout
R1=fit(G3~.,por[H$tr,c(inputs,g3)],model="ctree")
P1=predict(R1,por[H$ts,c(inputs,g3)])
target1=por[H$ts,]$G3
e1=mmetric(target1,P1,metric=c("RMSE","MAE"))
error=paste("CIT, holdout: RMSE=",round(e1[1],2),", R2=",round(e1[2],2),sep="")
print(error)
mgraph(target1,P1,graph="RSC",Grid=10,main=error)

# k-fold cross-validation
R2=crossvaldata(G3~.,por[,c(inputs,g3)],fit,predict,ngroup=10,seed=123,model="ctree",task="reg")
P2=R2$cv.fit # k-fold predictions on full dataset
e2=mmetric(por$G3,P2,metric=c("RMSE","MAE"))
error2=paste("CIT, 10-fold: RMSE=",round(e2[1],2),", MAE=",round(e2[2],2),sep="")
print(error2)
mgraph(por$G3,P2,graph="RSC",Grid=10,main=error2)

# holdout
R1=fit(G3~.,por[H$tr,c(inputs,g3)],model="mlpe")
P1=predict(R1,por[H$ts,c(inputs,g3)])
target1=por[H$ts,]$G3
e1=mmetric(target1,P1,metric=c("RMSE","MAE"))
error=paste("MLPE, holdout: RMSE=",round(e1[1],2),", R2=",round(e1[2],2),sep="")
print(error)
mgraph(target1,P1,graph="RSC",Grid=10,main=error)

# k-fold cross-validation
R2=crossvaldata(G3~.,por[,c(inputs,g3)],fit,predict,ngroup=10,seed=123,model="mlpe",task="reg")
P2=R2$cv.fit # k-fold predictions on full dataset
e2=mmetric(por$G3,P2,metric=c("RMSE","MAE"))
error2=paste("MLPE, 10-fold: RMSE=",round(e2[1],2),", MAE=",round(e2[2],2),sep="")
print(error2)
mgraph(por$G3,P2,graph="RSC",Grid=10,main=error2)

# holdout
R1=fit(G3~.,por[H$tr,c(inputs,g3)],model="svm")
P1=predict(R1,por[H$ts,c(inputs,g3)])
target1=por[H$ts,]$G3
e1=mmetric(target1,P1,metric=c("RMSE","MAE"))
error=paste("SVM, holdout: RMSE=",round(e1[1],2),", R2=",round(e1[2],2),sep="")
print(error)
mgraph(target1,P1,graph="RSC",Grid=10,main=error)

# k-fold cross-validation
R2=crossvaldata(G3~.,por[,c(inputs,g3)],fit,predict,ngroup=10,seed=123,model="svm",task="reg")
P2=R2$cv.fit # k-fold predictions on full dataset
e2=mmetric(por$G3,P2,metric=c("RMSE","MAE"))
error2=paste("SVM, 10-fold: RMSE=",round(e2[1],2),", MAE=",round(e2[2],2),sep="")
print(error2)
mgraph(por$G3,P2,graph="RSC",Grid=10,main=error2)

# holdout
R1=fit(G3~.,por[H$tr,c(inputs,g3)],model="naive")
P1=predict(R1,por[H$ts,c(inputs,g3)])
target1=por[H$ts,]$G3
e1=mmetric(target1,P1,metric=c("RMSE","MAE"))
error=paste("NB, holdout: RMSE=",round(e1[1],2),", R2=",round(e1[2],2),sep="")
print(error)
mgraph(target1,P1,graph="RSC",Grid=10,main=error)

# k-fold cross-validation
R2=crossvaldata(G3~.,por[,c(inputs,g3)],fit,predict,ngroup=10,seed=123,model="naive",task="reg")
P2=R2$cv.fit # k-fold predictions on full dataset
e2=mmetric(por$G3,P2,metric=c("RMSE","MAE"))
error2=paste("NB, 10-fold: RMSE=",round(e2[1],2),", MAE=",round(e2[2],2),sep="")
print(error2)
mgraph(por$G3,P2,graph="RSC",Grid=10,main=error2)

# holdout
R1=fit(G3~.,por[H$tr,c(inputs,g3)],model="knn")
P1=predict(R1,por[H$ts,c(inputs,g3)])
target1=por[H$ts,]$G3
e1=mmetric(target1,P1,metric=c("RMSE","MAE"))
error=paste("KNN, holdout: RMSE=",round(e1[1],2),", R2=",round(e1[2],2),sep="")
print(error)
mgraph(target1,P1,graph="RSC",Grid=10,main=error)

# k-fold cross-validation
R2=crossvaldata(G3~.,por[,c(inputs,g3)],fit,predict,ngroup=10,seed=123,model="knn",task="reg")
P2=R2$cv.fit # k-fold predictions on full dataset
e2=mmetric(por$G3,P2,metric=c("RMSE","MAE"))
error2=paste("KNN, 10-fold: RMSE=",round(e2[1],2),", MAE=",round(e2[2],2),sep="")
print(error2)
mgraph(por$G3,P2,graph="RSC",Grid=10,main=error2)

# holdout
R1=fit(G3~.,por[H$tr,c(inputs,g3)],model="lr")
P1=predict(R1,por[H$ts,c(inputs,g3)])
target1=por[H$ts,]$G3
e1=mmetric(target1,P1,metric=c("RMSE","MAE"))
error=paste("LR, holdout: RMSE=",round(e1[1],2),", R2=",round(e1[2],2),sep="")
print(error)
mgraph(target1,P1,graph="RSC",Grid=10,main=error)

# k-fold cross-validation
R2=crossvaldata(G3~.,por[,c(inputs,g3)],fit,predict,ngroup=10,seed=123,model="lr",task="reg")
P2=R2$cv.fit # k-fold predictions on full dataset
e2=mmetric(por$G3,P2,metric=c("RMSE","MAE"))
error2=paste("LR, 10-fold: RMSE=",round(e2[1],2),", MAE=",round(e2[2],2),sep="")
print(error2)
mgraph(por$G3,P2,graph="RSC",Grid=10,main=error2)

# holdout
R1=fit(G3~.,por[H$tr,c(inputs,g3)],model="mr")
P1=predict(R1,por[H$ts,c(inputs,g3)])
target1=por[H$ts,]$G3
e1=mmetric(target1,P1,metric=c("RMSE","MAE"))
error=paste("MR, holdout: RMSE=",round(e1[1],2),", R2=",round(e1[2],2),sep="")
print(error)
mgraph(target1,P1,graph="RSC",Grid=10,main=error)

# k-fold cross-validation
R2=crossvaldata(G3~.,por[,c(inputs,g3)],fit,predict,ngroup=10,seed=123,model="mr",task="reg")
P2=R2$cv.fit # k-fold predictions on full dataset
e2=mmetric(por$G3,P2,metric=c("RMSE","MAE"))
error2=paste("MR, 10-fold: RMSE=",round(e2[1],2),", MAE=",round(e2[2],2),sep="")
print(error2)
mgraph(por$G3,P2,graph="RSC",Grid=10,main=error2)

# holdout
R1=fit(G3~.,por[H$tr,c(inputs,g3)],model="randomForest")
P1=predict(R1,por[H$ts,c(inputs,g3)])
target1=por[H$ts,]$G3
e1=mmetric(target1,P1,metric=c("RMSE","MAE"))
error=paste("RF, holdout: RMSE=",round(e1[1],2),", R2=",round(e1[2],2),sep="")
print(error)
mgraph(target1,P1,graph="RSC",Grid=10,main=error)

# k-fold cross-validation
R2=crossvaldata(G3~.,por[,c(inputs,g3)],fit,predict,ngroup=10,seed=123,model="randomForest",task="reg")
P2=R2$cv.fit # k-fold predictions on full dataset
e2=mmetric(por$G3,P2,metric=c("RMSE","MAE"))
error2=paste("RF, 10-fold: RMSE=",round(e2[1],2),", MAE=",round(e2[2],2),sep="")
print(error2)
mgraph(por$G3,P2,graph="RSC",Grid=10,main=error2)

dev.off()
sink()