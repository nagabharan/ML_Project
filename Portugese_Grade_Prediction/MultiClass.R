if (!require("rminer")) 
{
    install.packages("rminer", repos="http://cran.rstudio.com/") 
    library("rminer")
}

# read previously saved file
por=read.table(file="dataset/por2.csv",header=TRUE)

# select inputs:
inputs=2:29 # select from 2 ("sex") to 29 ("health")

# select outputs: multiclass task "five"
cout=which(names(por)=="five")

cpor=por[,c(inputs,cout)] # for easy typing, new data.frame

cat("output class:",class(cpor$five),"\n")

# results function:
showres=function(M,data,output,cn)
{
    output=which(names(data)==output)
    Y=data[,output] # target values
    P=predict(M,data) # prediction values
    acc=round(mmetric(Y,P,metric="ACC"),2) # get accuracy
    cat(class(M@object),"> time elapsed:",M@time,", Global Accuracy:",acc,"\n")
    cat("Acc. per class:",round(mmetric(Y,P,metric="ACCLASS"),2),"\n")
    m=mmetric(Y,P,metric=c("CONF")) # a)
    cat("Confusion Mat:")
    print(m$conf) # confusion matrix
    cat("All Metrics:")
    m=mmetric(Y,P,metric=c("ALL"))
    print(round(m,1)) # all pure class metrics
    
    # ROC curve for class "A"
    TC=1
    txt=paste(cn,"AUC:",round(mmetric(Y,P,metric="AUC",TC=TC),2))
    mgraph(Y,P,graph="ROC",baseline=TRUE,Grid=10,main=txt,TC=-1)
}

pdf("graphs/por-multi-roc.pdf")
par(mfrow=c(1,1))
sink("Por-MultiClass-Output.txt")
# DT example:
C1=fit(five~.,cpor,model="rpart") # fit a decision tree
# print(C1@object)
cat("DT Acc:")
showres(C1,cpor,"five","DT")

# CIT example:
C2=fit(five~.,cpor,model="ctree") # fit a conditional inference tree
# print(C2@object)
cat("\nConditional Inference Tree Acc:")
showres(C2,cpor,"five","CITree")

# MLPE example:
C3=fit(five~.,cpor,model="mlpe") # fit a multilayer perceptron ensemble
# print(C3@object)
cat("\nMultiple Perceptron ensemble Acc:")
showres(C3,cpor,"five","MLPE")

# SVM example:
C4=fit(five~.,cpor,model="ksvm") # fit a support vector machine
# print(C4@object)
cat("\nSVM Acc:")
showres(C4,cpor,"five","SVM")

# NB example:
C5=fit(five~.,cpor,model="naive") # fit a naive bayes
# print(C5@object)
cat("\nNaive Bayes Acc:")
showres(C5,cpor,"five","NB")

# kNN example:
C6=fit(five~.,cpor,model="knn") # fit a knn
# print(C6@object)
cat("\nkNN Acc:")
showres(C6,cpor,"five","KNN")

# bagging example:
C7=fit(five~.,cpor,model="bagging") # fit bagging
# print(C7@object)
cat("\nBagging Acc:")
showres(C7,cpor,"five","Bagging")

# boosting example:
C8=fit(five~.,cpor,model="boosting") # fit boosting
# print(C8@object)
cat("\nBoosting Acc:")
showres(C8,cpor,"five","Boosting")

# randomForest example:
C9=fit(five~.,cpor,model="randomForest") # fit a random forest
# print(C9@object)
cat("\nRF Acc:")
showres(C9,cpor,"five","RF")

dev.off()
sink()

# save models to a file:
print("save C1 to file")
savemodel(C1,"models/multi/por-dt-mult.model") # saves to file

print("save C2 to file")
savemodel(C2,"models/multi/por-ctree-mult.model") # saves to file

print("save C3 to file")
savemodel(C3,"models/multi/por-mlpe-mult.model") # saves to file

print("save C4 to file")
savemodel(C4,"models/multi/por-svm-mult.model") # saves to file

print("save C5 to file")
savemodel(C5,"models/multi/por-naive-mult.model") # saves to file

print("save C6 to file")
savemodel(C6,"models/multi/por-knn-mult.model") # saves to file

print("save C7 to file")
savemodel(C7,"models/multi/por-bagging-mult.model") # saves to file

print("save C8 to file")
savemodel(C8,"models/multi/por-boosting-mult.model") # saves to file

print("save C9 to file")
savemodel(C9,"models/multi/por-rf-mult.model") # saves to file
