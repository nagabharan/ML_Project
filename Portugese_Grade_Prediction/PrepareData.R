# Prepare Target Column G3 for classification 
por=read.table(file="dataset/por.csv",header=TRUE) # read previously saved file

# binary class:
pass=cut(por$G3,c(-1,9,20),c("fail","pass"))

# five-level class:
five=cut(por$G3,c(-1,9,11,13,15,20),c("F","D","C","B","A")) # 5 grades

# create pdf:
pdf("graphs/por-histogram-grades.pdf")

par(mfrow=c(1,3))

plot(pass,main="Binary Class")

plot(five,main="5 Classes")

hist(por$G3,col="gray",main="Target G3",xlab="")

dev.off() # end of pdf creation

# creating the full dataset:
d=cbind(por,pass,five)

write.table(d,"dataset/por2.csv",row.names=FALSE,col.names=TRUE) # save to file