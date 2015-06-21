#---------Merge training and test sets---------
X <- rbind(read.table("c:/UCI HAR Dataset/train/X_train.txt"), read.table("c:/UCI HAR Dataset/test/X_test.txt"))
Subject <- rbind(read.table("c:/UCI HAR Dataset/train/subject_train.txt"), read.table("c:/UCI HAR Dataset/test/subject_test.txt"))
Y <- rbind(read.table("c:/UCI HAR Dataset/train/y_train.txt"), read.table("c:/UCI HAR Dataset/test/y_test.txt"))
#---------Extracts measurements mean and standard deviation---------
features <- read.table("c:/UCI HAR Dataset/features.txt")
interestingFeatures <- features[grep("(mean|std)\\(", features[,2]),]
mean_and_std <- X[,interestingFeatures[,1]]
#---------Descriptive activity names---------
activities <- read.table("c:/UCI HAR Dataset/activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) = "activity"
#---------Descriptive activity labels---------
names(Subject) <- "subject"
cleaned <- cbind(Subject, Y, mean_and_std)
#---------Tidy data set with the average of each variable for each activity and each subject---------
uniqueSubjects = unique(Subject)[,1]
Cols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]
count = 1
for (s in 1:length(unique(Subject)[,1])){
  for (a in 1:length(activities[,1])) {
    result[count, 1] = uniqueSubjects[s]
    result[count, 2] = activities[a, 2]
    tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
    result[count, 3:Cols] <- colMeans(tmp[, 3:Cols])
    count = count+1
  }
}
write.table(result, "c:/UCI HAR Dataset/tidy_data.txt")
#---------------------------------------------------------------------------------
