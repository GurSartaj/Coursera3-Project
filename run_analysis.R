## Getting and Cleaning Data Course Project

# Load Packages and get the Data
install.packages("data.table")
install.packages("dplyr")
install.packages("tidyr")
library(XML)
library(data.table)
library(dplyr)
library(tidyr)

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(getwd(), "dataset.zip"))
unzip(zipfile = "dataset.zip")

##Assigning dataframes and reading Data

#Features data
features <- read.table("UCI HAR Dataset/features.txt", header=FALSE, sep=" ", col.names = c("index", "functions"))  
head(features)

activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
head(activities)

#Train data
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")

head(subject_test)
glimpse(x_test)
head(y_test)

#Test data
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

head(subject_train)
glimpse(x_train)
head(y_train)

# Merges the training and the test sets to create one data set

x_data <- rbind(x_train, x_test)
y_data <- rbind(y_train, y_test)
subject_data <- rbind(subject_train, subject_test)
merged_data <- cbind(subject_data, x_data, y_data)

str(merged_data)
merged_data <- tbl_df(merged_data)

# Extracts only the measurements on the mean and standard deviation for each measurement

TidyData <- Merged_Data %>% select(subject, code, contains("mean"), contains("std")) #subset of the original 'data' that only contains mean and standard deviation coloumns for each measurement
glimpse(TidyData) # checking the TidyData dataset

#Uses descriptive activity names to name the activities in the data set

TidyData$code <- activities[TidyData$code, 2] # replacing codes to their respective activity names
head(TidyData) # checking the data

#Appropriately labels the data set with descriptive variable names.

names(TidyData)[2] = "activity"                                        # renaming "code" to "activity"
names(TidyData)<-gsub("Acc", "Accelerometer", names(TidyData))
names(TidyData)<-gsub("Gyro", "Gyroscope", names(TidyData))
names(TidyData)<-gsub("BodyBody", "Body", names(TidyData))
names(TidyData)<-gsub("Mag", "Magnitude", names(TidyData))
names(TidyData)<-gsub("^t", "Time", names(TidyData))
names(TidyData)<-gsub("^f", "Frequency", names(TidyData))
names(TidyData)<-gsub("tBody", "TimeBody", names(TidyData))
names(TidyData)<-gsub("-mean()", "Mean", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-std()", "STD", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-freq()", "Frequency", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("angle", "Angle", names(TidyData))
names(TidyData)<-gsub("gravity", "Gravity", names(TidyData))

TidyData <- tbl_df(TidyData) #converting the data frame to a dplyr data frame tibble
head(TidyData)
names(TidyData)

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
FinalData <- TidyData %>% group_by(subject, activity) %>% summarise_all(funs(mean))%>% print # grouping by subject and activity
write.table(FinalData, "FinalData.txt", row.name=FALSE) #writing the final output to "FinalData.txt"

str(FinalData) # crosschecking the final datset
View(FinalData) # view the final tidy dataset as a table using the "View" dplyr function
