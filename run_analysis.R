

library(dplyr)

###################
# Download fle 
#######################

zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
        download.file(zipUrl, zipFile, mode = "wb")
}



##########
# Read Data
##########

# read training data
train_subject <- read.table(file.path("train", "subject_train.txt"))
x_train_values <- read.table(file.path("train", "X_train.txt"))
y_train_activity <- read.table(file.path("train", "y_train.txt"))

# read test data
test_subject <- read.table(file.path("test", "subject_test.txt"))
x_test_values <- read.table(file.path("test", "X_test.txt"))
y_test_activity <- read.table(file.path("test", "y_test.txt"))

# read features as is
features <- read.table(("features.txt"), as.is = TRUE)

# read activity labels
activities <- read.table("activity_labels.txt")
colnames(activities) <- c("activityId", "activityLabel")

################
# Merge training and test data in one
#####################################

merged_activity <- rbind(
        cbind(train_subject, x_train_values, y_train_activity),
        cbind(test_subject, x_test_values, y_test_activity))

# Assign columns
colnames(merged_activity) <- c("subject", features[, 2], "activity")

######################
# subset mean and standard deviation 
###############################

# Identify which columns to keep
Keep_col <- grepl("subject|activity|mean|std", colnames(merged_activity))
#subset data with identified columns of mean and std
merged_activity <- merged_activity[ , Keep_col]

#########################
# Replace activity names
#########################

merged_activity$activity <- factor(merged_activity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])

# label data set with descriptive names
merged_activity_cols <- colnames(merged_activity) 

merged_activity_cols <- gsub("[\\(\\)-]", "", merged_activity_cols)

# clean up names
merged_activity_cols <- gsub("^f", "frequencyDomain", merged_activity_cols)
merged_activity_cols <- gsub("^t", "timeDomain", merged_activity_cols)
merged_activity_cols <- gsub("Acc", "Accelerometer", merged_activity_cols)
merged_activity_cols <- gsub("Gyro", "Gyroscope", merged_activity_cols)
merged_activity_cols <- gsub("Mag", "Magnitude", merged_activity_cols)
merged_activity_cols <- gsub("Freq", "Frequency", merged_activity_cols)
merged_activity_cols <- gsub("mean", "Mean", merged_activity_cols)
merged_activity_cols <- gsub("std", "StandardDeviation", merged_activity_cols)
merged_activity_cols <- gsub("BodyBody", "Body", merged_activity_cols)

# replace with new labels
colnames(merged_activity) <- merged_activity_cols

############################################################
# creates a second, independent tidy data set with the average
# of each variable for each activity and each subject
#############################################################

merged_activity_mean <- merged_activity %>%
        group_by(subject, activity) %>%
        summarise_each(funs(mean))

# write to text file
write.table(merged_activity_mean, "TidyData.txt", row.names = FALSE, quote = FALSE)

