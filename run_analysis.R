##read features and activity labels files

features<-read.table("C:/Aylin/UCI HAR Dataset/features.txt")
activ_labels<-read.table("C:/Aylin/UCI HAR Dataset/activity_labels.txt")


#read X, Y and subject files

readFiles<-function(xf,yf,sbf){
  xfile<-read.table(xf,header=F)
  yfile<-read.table(yf,header=F)
  sbf<-read.table(sbf,header=F)
  final<-cbind(xfile,yfile,sbf)
}
setwd("C:/Aylin/UCI HAR Dataset/")
testdata<-readFiles("X_test.txt","y_test.txt","subject_test.txt") ## read test data
traindata<-readFiles("X_train.txt","y_train.txt","subject_train.txt") ##read train data

## merge the training and the test sets to create one data set
data<-rbind(testdata,traindata) 
names(data)
col1561<-seq(1:561)
col1561<-as.character(col1561)
colnames(data)<-c(col1561,"Activity.Label","Subject")
names(data)

#extract only the measurements on the mean and standard deviation for each measurement
indexformean<-grep("mean",features[,2])
indexforstdev<-grep("std",features[,2])
finalindex<-c(indexformean,indexforstdev)
finalindex<-as.numeric(as.numeric(finalindex))
finalindex<-sort(finalindex)
finalindex<-as.character(finalindex)
finalData<-data[,c(finalindex,"Activity.Label","Subject")]
dim(finalData)

#use descriptive activity names to name the activities in the data set
finalData[,"Activity.Label"]<-as.numeric(finalData[,"Activity.Label"])
activ_labels[,1]<-as.numeric(activ_labels[,1])
names(activ_labels)
colnames(activ_labels)<-c("Label","Descr")
DataWDesc<-merge(finalData,activ_labels,by.x="Activity.Label",by.y="Label",sort=F)
DataWDesc[,"Descr"]<-gsub("_"," ",DataWDesc[,"Descr"])



##label the data set with descriptive variable names. 
NeedLabel<-names(DataWDesc[,2:80])
NL<-data.frame(NeedLabel)
features[,1]<-as.character(features[,1])
colnames(features)<-c("Label","Desc")
fix(features)
class(features[,2])
features[,2]<-as.character(features[,2])
features[,2]<-gsub("[()]","",features[,2])
ToBeUsedForColNames<-merge(NL,features,by.x="NeedLabel",by.y="Label",sort=F)
ToBeUsedForColNames[,2]<-as.character(ToBeUsedForColNames[,2])
colnames(DataWDesc)<-c("Activity.Label",ToBeUsedForColNames[,2],"Subject","Activity")
names(DataWDesc)
FD<-cbind(DataWDesc[,81:82],DataWDesc[,2:80])

## data set with the average of each variable for each activity and each subject

library(data.table)
FD[,"Subject"]<-as.factor(FD[,"Subject"])
FD[,"Activity"]<-as.factor(FD[,"Activity"])
FD<-data.table(FD)
avg<-FD[,lapply(.SD, mean),by = c("Subject","Activity")]
write.table(avg,file="tidyData.txt",row.names=F)

