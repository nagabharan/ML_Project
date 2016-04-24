if (!require("rminer")) 
{
  install.packages("rminer", repos="http://cran.rstudio.com/") 
  library("rminer")
}

# read previously saved file
math=read.table(file="math2.csv",header=TRUE)

# select inputs:
inputs=2:29 # select from 2 ("sex") to 29 ("health")

# select outputs: binary task "pass"
bout=which(names(math)=="pass")

cmath=math[,c(inputs,bout)] # for easy typing, new data.frame

cat("output class:",class(cmath$pass),"\n")

# results function:
showres=function(M,data,output,cn)
{
  output=which(names(data)==output)
  Y=data[,output] # target values
  P=predict(M,data) # prediction values
  acc=round(mmetric(Y,P,metric="ACC",TC=2,D=0.5),2) # get accuracy
  cat(class(M@object),"> time elapsed:",M@time,", Global Accuracy:",acc,"\n"
  )
  cat("Acc. per class:",round(mmetric(Y,P,metric="ACCLASS"),2),"\n")
  m=mmetric(Y,P,metric=c("CONF"),TC=2,D=0.1) # a)
  cat("Confusion Mat:")
  print(m$conf) # confusion matrix
  cat("All Metrics:")
  m=mmetric(Y,P,metric=c("ALL"))
  print(round(m,1)) # all pure class metrics
  
  txt=paste(cn,"AUC:",round(mmetric(Y,P,metric="AUC",TC=2),2))
  mgraph(Y,P,graph="ROC",baseline=TRUE,Grid=10,main=txt,TC=-1)
}

pdf("graphs/math-binary-roc.pdf")
par(mfrow=c(1,1))
sink("Math-Binary-Output.txt")
B1=fit(pass~.,math[,c(inputs,bout)],model="rpart",task="prob") # fit a decision tree
# print(B1@object)
cat("DT Acc:")
showres(B1,cmath,"pass","DT")

B2=fit(pass~.,math[,c(inputs,bout)],model="ctree",task="prob") # fit a conditional inference tree
# print(B2@object)
cat("\nConditional Inference Tree Acc:")
showres(B2,cmath,"pass","CITree")

B3=fit(pass~.,math[,c(inputs,bout)],model="mlpe",task="prob") # fit a multilayer perceptron ensemble
# print(B3@object)
cat("\nMultiple Perceptron ensemble Acc:")
showres(B3,cmath,"pass","MLPE")

B4=fit(pass~.,math[,c(inputs,bout)],model="ksvm",task="prob") # fit a supmatht vector machine
# print(B4@object)
cat("\nSVM Acc:")
showres(B4,cmath,"pass","SVM")

B5=fit(pass~.,math[,c(inputs,bout)],model="naive",task="prob") # fit a naive bayes
# print(B5@object)
cat("\nNaive Bayes Acc:")
showres(B5,cmath,"pass","NB")

B6=fit(pass~.,math[,c(inputs,bout)],model="knn",task="prob") # fit a knn
# print(B6@object)
cat("\nkNN Acc:")
showres(B6,cmath,"pass","KNN")

B7=fit(pass~.,math[,c(inputs,bout)],model="bagging",task="prob") # fit bagging
# print(B7@object)
cat("\nBagging Acc:")
showres(B7,cmath,"pass","Bagging")

B8=fit(pass~.,math[,c(inputs,bout)],model="boosting",task="prob") # fit boosting
# print(B8@object)
cat("\nBoosting Acc:")
showres(B8,cmath,"pass","Boosting")

B9=fit(pass~.,math[,c(inputs,bout)],model="randomForest",task="prob") # fit a random forest
# print(B9@object)
cat("\nRF Acc:")
showres(B9,cmath,"pass","RF")
dev.off()
sink()

# plot DT to file:
pdf("graphs/math-binary-DT.pdf")
plot(B1@object,uniform=TRUE,branch=0,compress=TRUE)
text(B1@object,xpd=TRUE,fancy=TRUE,fwidth=0.2,fheight=0.2)
dev.off()

# plot CI Tree to file:
pdf("graphs/math-binary-CITree.pdf")
plot(B2@object)
dev.off()

# plot RF to file:
pdf("graphs/math-binary-RF.pdf")
plot(B9@object)
dev.off()

# save models to a file:
print("save B1 to file")
savemodel(B1,"models/binary/math-dt-pass.model") # saves to file

print("save B2 to file")
savemodel(B2,"models/binary/math-ctree-pass.model") # saves to file

print("save B3 to file")
savemodel(B3,"models/binary/math-mlpe-pass.model") # saves to file

print("save B4 to file")
savemodel(B4,"models/binary/math-svm-pass.model") # saves to file

print("save B5 to file")
savemodel(B5,"models/binary/math-naive-pass.model") # saves to file

print("save B6 to file")
savemodel(B6,"models/binary/math-knn-pass.model") # saves to file

print("save B7 to file")
savemodel(B7,"models/binary/math-bagging-pass.model") # saves to file

print("save B8 to file")
savemodel(B8,"models/binary/math-boosting-pass.model") # saves to file

print("save B9 to file")
savemodel(B9,"models/binary/math-rf-pass.model") # saves to file