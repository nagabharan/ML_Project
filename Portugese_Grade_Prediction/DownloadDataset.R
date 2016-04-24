# simple show rows x columns function
nelems=function(d) paste(nrow(d),"x",ncol(d))

# student performance dataset (in zip file):
URL="http://archive.ics.uci.edu/ml/machine-learning-databases/00320/student.zip"

temp=tempfile() # temporary file

download.file(URL,temp) # download file to temporary

# Prepare Portugese file
# unzip file and load into data.frame:
por=read.table(unz(temp,"student-por.csv"),sep=";",header=TRUE)

cat("student performance por:",nelems(por),"\n")

print(class(por)) # show class

print(names(por)) # show attributes

# save data.frame to csv file:
write.table(por,file="dataset/por.csv",row.names=FALSE,col.names=TRUE)

# Prepare Math file
math=read.table(unz(temp,"student-mat.csv"),sep=";",header=TRUE)

cat("student performance math:",nelems(math),"\n")

print(class(math)) # show class

print(names(math)) # show attributes

# save data.frame to csv file:
write.table(math,file="dataset/math.csv",row.names=FALSE,col.names=TRUE)