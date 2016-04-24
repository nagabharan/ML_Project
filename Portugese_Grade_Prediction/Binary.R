if (!require("rminer")) 
{
    install.packages("rminer", repos="http://cran.rstudio.com/") 
    library("rminer")
}

# read previously saved file
por=read.table(file="dataset/por2.csv",header=TRUE)

# select inputs:
inputs=2:29 # select from 2 ("sex") to 29 ("health")

# select outputs: binary task "pass"
bout=which(names(por)=="pass")

cpor=por[,c(inputs,bout)] # for easy typing, new data.frame

cat("output class:",class(cpor$pass),"\n")

# results function:
showres=function(M,data,output)
{
    output=which(names(data)==output)
    Y=data[,output] # target values
    P=predict(M,data) # prediction values
    acc=round(mmetric(Y,P,metric="ACC"),2) # get accuracy
    cat(class(M@object),"> time elapsed:",M@time,", Global Accuracy:",acc,"\n"
    )
    cat("Acc. per class:",round(mmetric(Y,P,metric="ACCLASS"),2),"\n")
    m=mmetric(Y,P,metric=c("CONF")) # a)
    cat("Confusion Mat:")
    print(m$conf) # confusion matrix
    cat("All Metrics:")
    m=mmetric(Y,P,metric=c("ALL"))
    print(round(m,1)) # all pure class metrics
}

sink("Por-Binary-Output.txt")
B1=fit(pass~.,por[,c(inputs,bout)],model="rpart",task="class") # fit a decision tree
# print(B1@object)
cat("DT Acc:")
showres(B1,cpor,"pass")

B2=fit(pass~.,por[,c(inputs,bout)],model="ctree",task="class") # fit a conditional inference tree
# print(B2@object)
cat("\nConditional Inference Tree Acc:")
showres(B2,cpor,"pass")

B3=fit(pass~.,por[,c(inputs,bout)],model="mlpe",task="class") # fit a multilayer perceptron ensemble
# print(B3@object)
cat("\nMultiple Perceptron ensemble Acc:")
showres(B3,cpor,"pass")

B4=fit(pass~.,por[,c(inputs,bout)],model="ksvm",task="class") # fit a support vector machine
# print(B4@object)
cat("\nSVM Acc:")
showres(B4,cpor,"pass")

B5=fit(pass~.,por[,c(inputs,bout)],model="naive",task="class") # fit a naive bayes
# print(B5@object)
cat("\nNaive Bayes Acc:")
showres(B5,cpor,"pass")

B6=fit(pass~.,por[,c(inputs,bout)],model="knn",task="class") # fit a knn
# print(B6@object)
cat("\nkNN Acc:")
showres(B6,cpor,"pass")

B7=fit(pass~.,por[,c(inputs,bout)],model="bagging",task="class") # fit bagging
# print(B7@object)
cat("\nBagging Acc:")
showres(B7,cpor,"pass")
        
B8=fit(pass~.,por[,c(inputs,bout)],model="boosting",task="class") # fit boosting
# print(B8@object)
cat("\nBoosting Acc:")
showres(B8,cpor,"pass")

B9=fit(pass~.,por[,c(inputs,bout)],model="randomForest",task="class") # fit a random forest
# print(B9@object)
cat("\nRF Acc:")
showres(B9,cpor,"pass")
sink()

# plot DT to file:
pdf("graphs/por-binary-DT.pdf")
plot(B1@object,uniform=TRUE,branch=0,compress=TRUE)
text(B1@object,xpd=TRUE,fancy=TRUE,fwidth=0.2,fheight=0.2)
dev.off()

# plot CI Tree to file:
pdf("graphs/por-binary-CITree.pdf")
plot(B2@object)
dev.off()

# plot RF to file:
pdf("graphs/por-binary-RF.pdf")
plot(B9@object)
dev.off()

# save models to a file:
print("save B1 to file")
savemodel(B1,"models/binary/por-dt-pass.model") # saves to file

print("save B2 to file")
savemodel(B2,"models/binary/por-ctree-pass.model") # saves to file

print("save B3 to file")
savemodel(B3,"models/binary/por-mlpe-pass.model") # saves to file

print("save B4 to file")
savemodel(B4,"models/binary/por-svm-pass.model") # saves to file

print("save B5 to file")
savemodel(B5,"models/binary/por-naive-pass.model") # saves to file

print("save B6 to file")
savemodel(B6,"models/binary/por-knn-pass.model") # saves to file

print("save B7 to file")
savemodel(B7,"models/binary/por-bagging-pass.model") # saves to file

print("save B8 to file")
savemodel(B8,"models/binary/por-boosting-pass.model") # saves to file

print("save B9 to file")
savemodel(B9,"models/binary/por-rf-pass.model") # saves to file