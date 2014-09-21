## Step 1. Merges the training and the test sets to create one data set.
## Step 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## Step 3. Uses descriptive activity names to name the activities in the data set
## Step 4. Appropriately labels the data set with descriptive variable names. 
## Step 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

rm(list=ls())


## Step 1 : let's create a single dataset

## So first we upload the datasets in the environment

trainX <- read.table('./train/X_train.txt',header=FALSE)
testX <- read.table('./test/X_test.txt',header=FALSE)

trainy <- read.table('./train/y_train.txt',header=FALSE)
testy <- read.table('./test/y_test.txt',header=FALSE)

subject_train <- read.table('./train/subject_train.txt',header=FALSE)
subject_test <- read.table('./test/subject_test.txt',header=FALSE)

## Then we can merge data ...

mergedtrain <- cbind(trainy, subject_train, trainX)
mergedtest <- cbind(testy, subject_test, testX)

merged <- rbind(mergedtrain, mergedtest)

## ... and name them

colnames(merged)[1] <- "activity"
colnames(merged)[2] <- "subjects"

features <- read.table('./features.txt',header=FALSE)

features[,2] <- as.character(features[,2])

for (i in 1:nrow(features)) {
        colnames(merged)[i+2] <- features[i,2]
}


## Step 2 : Extracts only the measurements on the mean and standard deviation for each measurement. 

## First we need to identify the mean or std features, the function grepl will help

neededfeatures <- features[grepl("mean\\(\\)", features[,2]) | grepl("std\\(\\)", features[,2]), ] ## I saw on the forum that there is a debate about "should we also take into account the "meanFreq" features. Trent answer that it was not part of the requested data, so I don't look for them here.

neededfeatures[,2] <- as.character(neededfeatures[,2])

##neededfeatures give me the row number of each needed features. That number almost corresponds to the colum I look for in my "merged" data set : i have to add them "2" to take into account the "activity" and "sbujects" column.

subset <- merged[, c("activity", "subjects", neededfeatures[,2])]


## Step 3 : Uses descriptive activity names to name the activities in the data set.

## I'm using the subset created during the Step 2. But let's first get those activities :

activities <- read.table("./activity_labels.txt", header = FALSE)

activities[,2] <- as.character(activities[,2])

## There are probably much easier way to do this (with the "merge" function ?) but I was in a mood to use a "for loop" :

for (i in 1:nrow(subset)) {
        subset$activity[i] <- activities[(subset$activity[i]),2]
        }
rm(i)


## Step 4 : Appropriately labels the data set with descriptive variable names. 

## Well, we already made sure that all columns of the "subset" data set are labelled :

names(subset)


## Step 5 : From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Let's make it easier using the dplyr package

library(dplyr)

newset <- aggregate(subset, by = list (subject = subset$subjects, activity = subset$activity), FUN = mean, na.rm = TRUE)

newset <- newset[,c(1,2, 5:70)]

write.table(file = "tidyset.txt", row.name = FALSE, newset)
