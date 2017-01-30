library(dplyr)
library(stringr)

## Combining training Data set

## finding all training text files is train folder including subfoler to include acceleration signal 
file_list <- sort(list.files("./train",pattern="*.txt",recursive=TRUE),decreasing = TRUE)

## Deleting traindataset if it exists already in workspace
if (exists("traindataset")) { rm(traindataset) }

for (file in file_list){
        trainfile <- paste("./train/",file,sep="")
        # if the merged dataset doesn't exist, create it
        if (!exists("traindataset")){
                traindataset <- read.table(trainfile, header=FALSE)
        } else
                # if the merged dataset does exist, append to it columnwise
                if (exists("traindataset")){
                        temp_traindataset <-read.table(trainfile, header=FALSE)
                        traindataset<-cbind(traindataset, temp_traindataset)
                        rm(temp_traindataset)
                }
}

## Extracting feature names for training data set
features <- read.table("features.txt", header = FALSE)

## naming and assigning training data set column with unique names
names <- c("activity",as.character(paste(features[,1],features[,2])),"subject",paste(rep(c(substr(file_list[4:12],18,28)),each=128),rep(1:128)))
colnames(traindataset) = names

## Assigning a key "train" to training data set (a possible future key in some analysis)
traindataset$type <- "train"

## Combining Testing Data set

## finding all training text files is test folder including subfoler to include acceleration signal 
file_list <- sort(list.files("./test",pattern="*.txt",recursive=TRUE),decreasing = TRUE)

## Deleting testdataset if it exists already in workspace
if (exists("testdataset")) { rm(testdataset) }

for (file in file_list){
        
        testfile <- paste("./test/",file,sep="")
        # if the merged dataset doesn't exist, create it
        if (!exists("testdataset")){
                testdataset <- read.table(testfile, header=FALSE)
        } else
                # if the merged dataset does exist, append to it
                if (exists("testdataset")){
                        temp_testdataset <-read.table(testfile, header=FALSE)
                        testdataset<-cbind(testdataset, temp_testdataset)
                        rm(temp_testdataset)
                }
}

## Extracting feature names for testing data set
features <- read.table("features.txt", header = FALSE)

## naming and assigning testing data set column with unique names
names <- c("activity",as.character(paste(features[,1,],features[,2])),"subject",paste(rep(c(substr(file_list[4:12],18,28)),each=128),rep(1:128)))
colnames(testdataset) = names

# Assigning a key "test" to training data set (a possible future key in some analysis)
testdataset$type <- "test"

## Merging training & testing data set

traintestdataset <- rbind(traindataset,testdataset)

## slecting coulmns/measurement having mean() and std() in merged data set

ttmeansd <- select(traintestdataset,subject,activity,type,contains("mean()"),contains("std()"))

## Assigning descriptive activity names to name the activities in the data set

activity_labels <- read.table("activity_labels.txt", header = FALSE)
names(activity_labels) <- c("activity","activity_description")
ttmeansd_act_des <- merge(x=ttmeansd,y=activity_labels,by="activity",all.x=TRUE)

## Sequencing columns for a tidy look 

ttmeansd_act_des <- select(ttmeansd_act_des,subject,activity,activity_description,type,4:69)

## cleaningup column names & assiging them to the tidy data set

names_temp <- str_split_fixed(names(ttmeansd_act_des)," ",2)
names <- c(names_temp[1:4,1],names_temp[5:70,2])
colnames(ttmeansd_act_des) = names

## Tidy training data set with the average of each variable for each activity and each subject

tidydataset <- ttmeansd_act_des %>% 
        select(-activity,-type) %>%
        group_by(activity_description,subject) %>%
        summarise_each(funs(mean))

write.table(tidydataset,"./tidydataset.txt",sep="\t",row.names = FALSE)