* load dplyr
library(dplyr)

* getting and unzipping data
fichier <- "getdata_dataset.zip"
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
download.file(URL, fichier, method="curl")
unzip(fichier)

* x_test loading
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")

* loading features to get the names
features <- read.table("UCI HAR Dataset/features.txt")

* naming x_test like col2 of features
x_test <- setNames(x_test, features[,2])

* loading and naming y_test
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
names(y_test) <- "activity_label"

* loading and naming subject_test
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
names(subject_test) <- "subject"

* combining the 3 tests
testdf <- cbind(subject_test,y_test,x_test)

* same thing for the train dataset
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
y_train <- read.table("UCI HAR Dataset/train/Y_train.txt")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
names(y_train) <- "activity_label"
names(subject_train) <- "subject"
x_train <- setNames(x_train, features[,2])
traindf <- cbind(subject_train,y_train,x_train)

* combining test and train data frames
dfall <- rbind(testdf,traindf)

* extract only mean and std
dfsubact = subset(dfall,select = c(subject,activity_label))
dfmean <- dfall[ , grepl("mean\\(\\)", colnames(dfall))]
dfstd <- dfall[ , grepl("std()", colnames(dfall))]
dfmeanstd <-cbind(dfsubact,dfmean,dfstd)

* rename properly the activities
activity_txt <- read.table("UCI HAR Dataset/activity_labels.txt")
dfmeanstd$activity_label <- activity_txt[dfmeanstd$activity_label, 2]

* rename properly the variable names
names(dfmeanstd)<-gsub("tBodyAcc", "Time Body Accelerometer ", names(dfmeanstd))
names(dfmeanstd)<-gsub("tGravityAcc", "Time Gravity Accelerometer ", names(dfmeanstd))
names(dfmeanstd)<-gsub("tBodyGyro", "Time Body Gyroscope ", names(dfmeanstd))
names(dfmeanstd)<-gsub("tGravityGyro", "Time Gravity Gyroscope ", names(dfmeanstd))
names(dfmeanstd)<-gsub("BodyBody", "Body", names(dfmeanstd))
names(dfmeanstd)<-gsub("fBodyAcc", "Frequency Body Accelerometer ", names(dfmeanstd))
names(dfmeanstd)<-gsub("fBodyGyro", "Frequency Body Gyroscope ", names(dfmeanstd))
names(dfmeanstd)<-gsub("-mean()", "- Mean", names(dfmeanstd))
names(dfmeanstd)<-gsub("-std()", "- STD", names(dfmeanstd))

* calcutate means of each variables, activities and subject
dffinal <- dfmeanstd %>% group_by(subject, activity_label) %>% summarise_all(list(mean = mean))%>% ungroup()

* generating the file
write.table(dffinal, "dataset.txt", row.names = FALSE)