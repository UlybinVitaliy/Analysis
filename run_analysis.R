library (dplyr)

##Load Data.
Subject_Train<-read.table("Dataset/train/subject_train.txt")
Subject_Test<-read.table("Dataset/test/subject_test.txt")
X_Train<-read.table("Dataset/train/X_train.txt")
X_Test<-read.table("Dataset/test/X_test.txt")
Y_Train<-read.table("Dataset/train/y_train.txt")
Y_Test<-read.table("Dataset/test/y_test.txt")
Table_F<-read.table("Dataset/features.txt")
Table_AL<-read.table("Dataset/activity_labels.txt")

##Merges the training and the test sets to create one data set.
Table_Subject<-rbind(Subject_Train,Subject_Test)
Table_X<-rbind(X_Train,X_Test)
Table_Y<-rbind(Y_Train,Y_Test)

    
##Extracts only the measurements on the mean and standard deviation for each measurement. 
Index_F<-grep("-mean\\(\\)|-std\\(\\)",Table_F$V2)
Table_X<-Table_X[,Index_F]
names(Table_X)<-Table_F[Index_F,2]
names(Table_X)<-gsub("\\(|\\)","",names(Table_X))
names(Table_X)<-tolower(names(Table_X))
    
##Uses descriptive activity names to name the activities in the data set
List_AL<-tolower(as.character(Table_AL$V2))
Table_AL$V2<-gsub("_","",List_AL)
Table_Y[,1]<-Table_AL[Table_Y[,1],2]
names(Table_Y)<-"activity"
    
##Appropriately labels the data set with descriptive variable names.
Table_Subject<-"subject"
ReadyData<-cbind(Table_Subject,Table_Y,Table_X) 

##From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
ReadyData$subject<-as.factor(ReadyData$subject)
ReadyData<-data.table(ReadyData)
tidy<-aggregate(. ~Subject + Activity,ReadyData,mean)
tidy<-tidy[order(tidy$subject,tidy$activity),]
write.table(tidy,file ="tidy.txt",row.names = FALSE)
