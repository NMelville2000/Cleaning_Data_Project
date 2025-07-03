library(dplyr)
library(readr)
getwd()


###Question 1

#Read all files individually into R
#Rename columns in each file
#Rbind then cbind files into one dataset


###FEATURES AND ACTIVITY LABELS
features <- read.table("Cleaning_Data_Project/UCI HAR Dataset/features.txt", col.names = c("Codes", "featuresList"))
activities <- read.table("Cleaning_Data_Project/UCI HAR Dataset/activity_labels.txt", col.names = c("Id", "activity_names"))


###TEST FOLDER
test_subject <- read.table("Cleaning_Data_Project/UCI HAR Dataset/test/subject_test.txt", col.names = c("subjects"))
X_test <- read.table("Cleaning_Data_Project/UCI HAR Dataset/test/X_test.txt", col.names = features$featuresList)
Y_test <- read.table("Cleaning_Data_Project/UCI HAR Dataset/test/y_test.txt", col.names = c("activity"))

###TRAIN FOLDER
train_subject <- read.table("Cleaning_Data_Project/UCI HAR Dataset/train/subject_train.txt", col.names = c("subjects"))
X_train <- read.table("Cleaning_Data_Project/UCI HAR Dataset/train/X_train.txt", col.names = features$featuresList)
y_train <- read.table("Cleaning_Data_Project/UCI HAR Dataset/train/y_train.txt", col.names = c("activity"))


#Row bind files first
r_bind_Subjects <- rbind(test_subject, train_subject)
r_bind_sets <- rbind(X_test, X_train)
r_bind_Codes <- rbind(Y_test, y_train)


#Column bind combined rows into one merged dataset
c_bind_all <- cbind(r_bind_Subjects, r_bind_Codes, r_bind_sets)

###Question 2 
##Extracts only the mean standard deviation for each measurement.
mean_stds <- grepl("mean\\(\\)|std\\(\\)", features$featuresList)
extracted_data <- c_bind_all[ , c(TRUE, TRUE, mean_stds)]


###Question 3
##Uses descriptive activity names to name the activities in the data set.
extracted_data$activity <- activities[extracted_data$activity, 2]


###Question 4
##Renaming the columns to appropriate labels of the variables.
names(extracted_data) <- gsub("Acc", "Accelerometer", names(extracted_data))
names(extracted_data) <- gsub("Gyro", "Gyroscope", names(extracted_data))
names(extracted_data) <- gsub("^t", "time", names(extracted_data))
names(extracted_data) <- gsub("^f", "frequency", names(extracted_data))
names(extracted_data) <- gsub("Mag", "Magnitude", names(extracted_data))


###Question 5
##Create a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_data <- extracted_data %>%
        group_by(subjects, activity) %>%
        summarise_all(list(mean))
                
View(tidy_data) 

###Creating a text file from the following code.
write.table(tidy_data, row.names = FALSE) 
       





