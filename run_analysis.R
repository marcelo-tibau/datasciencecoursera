#To read the files at both the training and the test sets.

filesPath <- ".data/UCI HAR Dataset"
data_Subject_Train <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
data_Subject_Test <- tbl_df(read.table(file.path(filesPath, "test", "subject_test.txt")))
data_Y_Train <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
data_Y_Test <- tbl_df(read.table(file.path(filesPath, "test", "Y_test.txt")))
data_X_Train <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt")))
data_X_Test <- tbl_df(read.table(file.path(filesPath, "test", "X_test.txt")))

#To merge both the training and the test sets to create one data set.

data_Subject <- rbind(data_Subject_Train, data_Subject_Test)
setnames(data_Subject, "V1", "subject")
data_Y <- rbind(data_Y_Train, data_Y_Test)
setnames(data_Y, "V1", "activity_num")
data_X <- rbind(data_X_Train, data_X_Test)

#Setting the name variables.
data_features <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(data_features, names(data_features), c("feature_num", "feature_name"))
colnames(data_X) <- data_features$feature_name

#Setting the column names for activity labels
activity_labels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activity_labels, names(activity_labels), c("activity_num","activity_name"))

# Merging the columns
all_data_subjact<- cbind(data_Subject, data_Y)
data_X <- cbind(all_data_subjact, data_X)

#To extract only the measurements on the mean and standard deviation for each measurement. 
data_features_mean_std_dev <- grep("mean\\(\\)|std\\(\\)",data_features$feature_name,value=TRUE)

# Taking the mean and standard deviation and adding "subject","activity_num"

data_features_mean_std_dev <- union(c("subject","activity_num"), data_features_mean_std_dev)
data_X <- subset(data_X, select = data_features_mean_std_dev)

# To use descriptive activity names to name the activities in the data set
# Entering name of activity into data_X
data_X <- merge(activity_labels, data_X, by="activity_num", all.x=TRUE)
data_X$activity_name <- as.character(data_X$activity_name)

# To create data_X with variable means sorted by subject and activity
data_X$activity_name <- as.character(data_X$activity_name)
data_aggr <- aggregate(. ~ subject - activity_name, data = data_X, mean) 
data_X <- tbl_df(arrange(data_aggr,subject,activity_name))

