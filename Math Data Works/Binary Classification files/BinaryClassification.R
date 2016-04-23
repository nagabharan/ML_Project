# simple show rows x columns function
nelems=function(d) paste(nrow(d),"x",ncol(d))

# student performance dataset (in zip file):
URL="http://archive.ics.uci.edu/ml/machine-learning-databases/00320/student.zip"
temp=tempfile() # temporary file
download.file(URL,temp) # download file to temporary
# unzip file and load into data.frame:
math=read.table(unz(temp,"student-mat.csv"),sep=";",header=TRUE)
cat("student performance math:",nelems(math),"\n")
print(class(math)) # show class
print(names(math)) # show attributes
# save data.frame to csv file:
write.table(math,file="math.csv",row.names=FALSE,col.names=TRUE)


#Data Preparation

# read from saved file
math=read.table(file="math.csv",header=TRUE) # read previously saved file

# binary task:
pass=cut(math$G3,c(-1,9,20),c("fail","pass"))
# five-level system:
five=cut(math$G3,c(-1,9,11,13,15,20),c("F","D","C","B","A")) 
# create pdf:
pdf("math-grades.pdf")
par(mfrow=c(1,3))
plot(pass,main="pass")
plot(five,main="five")
hist(math$G3,col="gray",main="G3",xlab="")
dev.off() # end of pdf creation
# creating the full dataset:
d=cbind(math,pass,five)
write.table(d,"math2.csv",row.names=FALSE,col.names=TRUE) # save to file

#The code above  produces a new data frame that is saved into file "math2.csv" and creates a pdf with
#the distinct output histograms (math-grades.pdf)


install.packages("rminer")

# BINARY CLASSIFICATION

library(rminer)

# read previously saved file
math=read.table(file="math2.csv",header=TRUE)

# select inputs:
inputs=2:29 # select from 2 ("sex") to 29 ("health")

# select outputs: binary task "pass"
bout=which(names(math)=="pass")
cat("output class:",class(math[,bout]),"\n")
#The formula defines the output (variable ”pass”) to be
#modeled ( ∼ ) from the inputs (. means all other data.frame variables). The data.frame includes
#the selected inputs and output variables. 



# two white-box examples:
B1=fit(pass∼.,math[,c(inputs,bout)],model="rpart") # fit a decision tree
print(B1@object)
pdf("trees-1.pdf")

# rpart functions:
plot(B1@object,uniform=TRUE,branch=0,compress=TRUE)
text(B1@object,xpd=TRUE,fancy=TRUE,fwidth=0.2,fheight=0.2)
dev.off()
B2=fit(pass∼.,math[,c(inputs,bout)],model="ctree") # fit a conditional inference tree
print(B1@object)
pdf("trees-2.pdf")


# ctree function:
plot(B2@object)
dev.off()


# two black-box examples:
B3=fit(pass∼.,math[,c(inputs,bout)],model="mlpe") # fit a multilayer perceptron ensemble
print(B3@object)
B4=fit(pass∼.,math[,c(inputs,bout)],model="ksvm") # fit a support vector
machine
print(B4@object)
# save one model to a file:
print("save B3 to file")
savemodel(B3,"mlpe-pass.model") # saves to file
print("load from file into B5")
B5=loadmodel("mlpe-pass.model") # load from file
print(class(B5@object$mlp[[1]]))





# MULTICLASS CLASSIFICATION




