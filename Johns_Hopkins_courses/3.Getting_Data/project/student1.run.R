library(dplyr)
###############################################################################
run_analysis <- function() {
###############################################################################
# Read in test and train datasets and combine into one dataset
testDataRaw <- tbl_df(read.table("UCI HAR Dataset/test/X_test.txt"))
testSub <- tbl_df(read.table("UCI HAR Dataset/test/subject_test.txt"))
testAct <- tbl_df(read.table("UCI HAR Dataset/test/Y_test.txt"))
trainDataRaw <- tbl_df(read.table("UCI HAR Dataset/train/X_train.txt"))
trainSub <- tbl_df(read.table("UCI HAR Dataset/train/subject_train.txt"))
trainAct <- tbl_df(read.table("UCI HAR Dataset/train/Y_train.txt"))
testData <- cbind(testSub, testAct, testDataRaw)
trainData <- cbind(trainSub, trainAct, trainDataRaw)
fullData <- rbind(testData, trainData)
# Create unique readable variable names and apply to full data set

browser();
varNames <- tbl_df(read.table("UCI HAR Dataset/features.txt"))
varNames <- select(varNames, V2)
varNames <- t(varNames)
varNamesUnique <- make.names(varNames, unique = TRUE)
colnames(fullData) <- c("Subject", "Activity", varNamesUnique)

                                        # Create 3 data frames: subject & activity columns, measurements containing
# "mean", and measurements containing "std"
subAndAct <- select(fullData, Subject:Activity)
meanDat <- select(fullData, contains("mean"))
stdDat <- select(fullData, contains("std"))

# Combine the 3 data frames
subDat <- cbind(subAndAct, meanDat, stdDat)
# Group data by subject and activity and get a mean score for each subject for
# each activity
cleanDat <- subDat %>%
group_by(Subject,Activity) %>%
summarise_each(funs(mean))
# Write table to a blank file
write.table(cleanDat, file = "Samsung_fitness_data.txt", row.names = FALSE)
}
