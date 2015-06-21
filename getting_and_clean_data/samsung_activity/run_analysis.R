# create a directory to save samsung data
if (!file.exists('./samsung')){
  dir.create('./samsung')
}

# download data set name it data.zip
#download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip', destfile = './data.zip', method='curl')
# unzip data into samsung folder, overwrite existing data
#unzip('./data.zip', exdir='./', overwrite=TRUE, unzip='unzip')

# merge training and testing dataset
# 1. read training data
x_train <- read.table('./samsung/UCI HAR Dataset/train/X_train.txt')
y_train <- read.table('./samsung/UCI HAR Dataset/train/Y_train.txt')
subject_train <- read.table('./samsung/UCI HAR Dataset/train/subject_train.txt')

# 2. read testing data
x_test <- read.table('./samsung/UCI HAR Dataset/test/X_test.txt')
y_test <- read.table('./samsung/UCI HAR Dataset/test/Y_test.txt')
subject_test <- read.table('./samsung/UCI HAR Dataset/test/subject_test.txt')

# 1. merge those data tables!
x_all <- rbind(x_train, x_test) # all the feature sets
y_all <- rbind(y_train, y_test) # all the labels
subject_all <- rbind(subject_train, subject_test)

# extract mean and standard deviation, giving descriptive names to column and rows
# acquire features label as column names
features <- read.table('./samsung/UCI HAR Dataset/features.txt')
mean_std_index <- grep('-(mean|std)\\(\\)', features[, 2])
mean_std <- features[mean_std_index, ]
# subset x_all using mean_std_index
x_all <- x_all[ , mean_std[, 1]]
# use descriptive mean_std_data column names
names(x_all) <- mean_std[, 2]

# read activity labels
activity_labels <- read.table('./samsung/UCI HAR Dataset/activity_labels.txt')
# rename lable to be more descriptive, 
y_all[ , 1] <- activity_labels[y_all[, 1], 2]
# rename lable column names
names(y_all) <- "activities"

# rename subject data
names(subject_all) <- 'subjects'

# 4 combine those data into one clean table with descriptive names 
# and only mean/standard deviation data
clean_data <- cbind(subject_all, y_all, x_all)

# 5 average of each variable for each activity and each subject.
# import dplyr for easier data manipulation
library('dplyr')
library('plyr')
avg_by_act_per_subj <- ddply(clean_data, .(subjects, activities), numcolwise(mean))
