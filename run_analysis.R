
## install and load packages data.table and reshape2
install.packages("gsubfn","data.table", "reshape2")
library("gsubfn","data.table", "reshape2")


## geting data from the zip file
path <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(path, "data.zip"))
unzip(zipfile = "data.zip")

## loading the activity_labels and features
activityLabels <- fread(file.path(path, "UCI HAR Dataset/activity_labels.txt"),
                        col.names = c("classLabels", "activityNames"))
## loading the features
features <-fread(file.path(path, "/UCI HAR Dataset/features.txt"),
                 col.names = c("index", "featureNames"))

## Extracts only the measurements on the mean and standard deviation for each measurement
featuresNeeded <- grep("(mean|std)\\(\\)", features[, featureNames])
measurements <- features[featuresNeeded, featureNames]
measurements <- gsubfn("(^t|^f|Acc|Gyro|Mag|BodyBody|\\(\\))",
  list("t" = "Time",
       "f" = "Frequency",
     "Acc" = "Accelerometer",
    "Gyro" = "Gyroscope",
     "Mag" = "Magnitude",
"BodyBody" = "Body",
      "()" = ""),
  measurements)

## Load the training data, including filtering using with=False
train <- fread(file.path(path, "/UCI HAR Dataset/train/X_train.txt"))[, featuresNeeded, with = FALSE]
setnames(train, colnames(train), measurements) # change column name based on measurement

activityTrain <- fread(file.path(path, "/UCI HAR Dataset/train/y_train.txt"), col.names = "Activity")
subjectTrain <-  fread(file.path(path, "/UCI HAR Dataset/train/subject_train.txt"), col.names = "SubjectNo.")

## Bind all columns together
train <- cbind(activityTrain, subjectTrain, train) 

## Load the testing data, including filtering using with=False
test <- fread(file.path(path, "/UCI HAR Dataset/test/X_test.txt"))[, featuresNeeded, with = FALSE]
setnames(test, colnames(test), measurements)

activityTest <- fread(file.path(path, "/UCI HAR Dataset/test/y_test.txt"), col.names = "Activity")
subjectTest <-  fread(file.path(path, "/UCI HAR Dataset/test/subject_test.txt"), col.names = "SubjectNo.")

## Bind all columns together
test <- cbind(activityTest, subjectTest, test) 

## Merges the training and the test sets to create one data set (by rows)
testTrain <- rbind(train, test)

## Uses descriptive activity names to name the activities in the data set, Factor "Activity"
testTrain[["Activity"]] <- factor(testTrain[, Activity], levels = activityLabels[["classLabels"]],
                                  labels = activityLabels[["activityNames"]])

## Use as.factor() in order to create turn subject numbers into factors
testTrain[["SubjectNo."]] <- as.factor(testTrain[, SubjectNo.])


# Melting (to variable and value) and then casting the data table (Average of SubjectNo and Activity)
testTrain <- melt.data.table(testTrain, id=c("SubjectNo.", "Activity"))
testTrain <- dcast(testTrain, SubjectNo. + Activity ~ variable, mean)

## Creates a second, independent tidy data set with the average of each variable for each activity and each subject
fwrite(testTrain, file="tidyData.txt")

write.table(testTrain, file="tidyData.txt", row.name=FALSE)
