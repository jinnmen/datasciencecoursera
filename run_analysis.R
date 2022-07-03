library(dplyr)
library(reshape2)

######## Load and clean data first ########
# Load names and labels files
xfeatures<- read.table("./UCI HAR Dataset/features.txt")
ylabels<-read.table("./UCI HAR Dataset/activity_labels.txt")

# Load test data files
xtest <- read.table("./UCI HAR Dataset/test/X_test.txt")
ytest <- read.table("./UCI HAR Dataset/test/y_test.txt")
subjecttest<-read.table("./UCI HAR Dataset/test/subject_test.txt")

# Check class and loaded properly
glimpse(features)
glimpse(xtest)
glimpse(ytest)
glimpse(subjecttest)

# Check to compare feature names are same length as xtest values
length(xfeatures[[2]])
length(xtest)

# Set header names for xtest with 2nd column of features
names(xtest) = xfeatures[[2]]

# Check 
glimpse(xtest)

# Check values of y
head(ylabels)
head(ytest)

# Assign labels to each value in a new 2nd column to y
ytest[[2]]<-ylabels[[2]][ytest[[1]]]

# Check
glimpse(ytest)

# Assign column names to ytest and subject test
colnames(ytest) <- c("activityid","activitylabel")
names(subjecttest)="subject"

# Check to ensure names assigned correctly
names(ytest)
names(subjecttest)

#merge all data above for test data
testdata<-cbind(subjecttest, ytest,xtest)

# Check
View(testdata)



# Repeat the same for train data

# Load train data files
xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
subjecttrain<-read.table("./UCI HAR Dataset/train/subject_train.txt")

# Check class and loaded properly
glimpse(features)
glimpse(xtrain)
glimpse(ytrain)
glimpse(subjecttrain)

# Check to compare feature names are same length as xtrain values
length(xfeatures[[2]])
length(xtrain)

# Set header names for xtrain with 2nd column of features
names(xtrain) = xfeatures[[2]]

# Check 
glimpse(xtest)

# Check values of y
head(ylabels)
head(ytrain)

# Assign labels to each value in a new 2nd column to y
ytrain[[2]]<-ylabels[[2]][ytrain[[1]]]

# Check
glimpse(ytrain)

# Assign column names to ytest and subject test
colnames(ytrain) <- c("activityid","activitylabel")
names(subjecttrain)="subject"

# Check to ensure names assigned correctly
names(ytrain)
names(subjecttrain)

#merge all data above for test data
traindata<-cbind(subjecttrain, ytrain,xtrain)

# Check
View(traindata)



######## Assignment questions and answers ########
#1. Merging both test and train data
ttdata<-rbind(testdata,traindata)

#Check
View(ttdata)

#Confirm rows merged correctly
length(testdata[[1]])
length(traindata[[1]])
length(ttdata[[1]])


#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# Find True values for columns then subset
meanstd<-grepl("[Mm]ean|[Ss]td", colnames(ttdata))

# Check
print(meanstd)

# Apply and select the column numbers to use
ttdatams<- ttdata[,(which(meanstd))]

#Check
glimpse(ttdatams)


#3. Uses descriptive activity names to name the activities in the data set
# Completed when loaded and cleaning data.
View(ttdata[[3]])

#4. Appropriately labels the data set with descriptive variable names. 
names(ttdata)

# From the README.txt and features_info.txt, we get some keywords:
## t= time. These time domain signals (prefix 't' to denote time) 
## acc = accelerometer. sensor signals (accelerometer and gyroscope)
## mag = magnitude. sma(): Signal magnitude area
## f = frequency 
## body = body. 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal...

# We can use lower casing to prevent mistakes when handling data
names(ttdata)=tolower(names(ttdata))

# Next we use regex to label the texts a bit more clearly
names(ttdata)<-sub("^t","time",names(ttdata))
names(ttdata)<-sub("acc","accelerometer",names(ttdata))
names(ttdata)<-sub("mag","magnitude",names(ttdata))
names(ttdata)<-sub("^f","frequency",names(ttdata))
names(ttdata)<-sub("bodybody","body",names(ttdata))
names(ttdata)<-sub("tbody","timebody",names(ttdata))

names(ttdata)

View(ttdata)


#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Merge subject and activity label to mean and std
tidydata0<-ttdata %>% select(subject,activitylabel)
tidydata<-cbind(tidydata0,ttdatams)        

#Check
glimpse(tidydata)

#Melt data from wide to long
tidydatam<-melt(tidydata, id=c("subject","activitylabel"))

glimpse(tidydatam)

# Next we aggregate the mean by casting from long to wide
tidydataw<-dcast(tidydatam, subject+activitylabel~variable, mean)

#Check
glimpse(tidydataw)
View(tidydataw)

#Save as file
write.table(tidydataw, file="tidydata.txt",row.names = FALSE)