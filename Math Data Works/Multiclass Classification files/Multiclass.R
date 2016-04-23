#For the five-level classification, three classification models are adopted, namely bagging, boosting
#and random forests


library(rminer)
# read previously saved file
math=read.table(file="math2.csv",header=TRUE)
# select inputs:
inputs=2:29 # select from 2 ("sex") to 29 ("health")
# select outputs: multiclass task "five"
cout=which(names(math)=="five")
cmath=math[,c(inputs,cout)] # for easy typing, new data.frame
cat("output class:",class(cmath$five),"\n")
# auxiliary function:
showres=function(M,data,output)
{
  output=which(names(data)==output)
  Y=data[,output] # target values
  P=predict(M,data) # prediction values
  acc=round(mmetric(Y,P,metric="ACC"),2) # get accuracy
  cat(class(M@object),"> time elapsed:",M@time,", Global Accuracy:",acc,"\n"
  )
  cat("Acc. per class",round(mmetric(Y,P,metric="ACCLASS"),2),"\n")
}
# bagging example:
C1=fit(five∼.,cmath,model="bagging") # bagging from adabag package
showres(C1,cmath,"five")
# boosting example:
C2=fit(five∼.,cmath,model="boosting") # boosting from adabag package
showres(C2,cmath,"five")
# randomForest example:
C3=fit(five∼.,cmath,model="randomForest") # from randomForest package
showres(C3,cmath,"five")
