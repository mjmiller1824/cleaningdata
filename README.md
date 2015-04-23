Run Analysis takes the original seven files and combines them into one large data set. Then it subsets the data by columns with "mean" and "std" in the title.
It splits the subsetted data by "subjectId" and "activityName". Then it calculates the mean of the data for each column puts it into long form
and puts it together in one data set with cleaned labels and outputs a new and cleaned data with just the means and labels.
 


From the original set of seven files the following actions were taken in order:
1.The titles were read in from "features.txt" and turned into a vector.
2.The x_test data is read into R.
3.The y_test data is read into R.
4.The subject_test data is read into R.
5.The x_train data is read into R.
6.The y_train data is read into R.
7.The subject_train data is read into R.
8.The names from "features.txt" are assigned to x_test.
9.The names from "features.txt" are assigned to x_train.
10.Combines the x_test and x_train datasets into one dataset via rbind.
11.Combines the y_test and y_train into one dataset via rbind
12.Names the y values as "activityname" for subsetting later
13.Replaces the activity numbers with activity labels from activity_labels.txt
14.Combines the subject_test and subject_train into one dataset via rbind.
15.Names the subject values as "subjectid" for subsetting later.
16.Subsets the columns that have mean or std in the name.
 I went this route to ensure all requested data was pulled.
17.The next line puts all of the read columns into one big data frame with assigned subject ids and activity names.
18.Assigns names to a varible that will be used to name 
the columns on the pre-meaned dataset. 
Here some explanation of choices in names is necessary.


###names for the pre meaned data set. These are provided so that if  
###someone wants to see the premeaned data they can see it.
###column names used to have - and () in them. These are problematic for R
###Where these characters existed, they have been replaced with "." in the middle of the name and omitted at the end.
###In the sections where there were multiple words strung together,
###camelCase has been used. These modifications were done to preserve readability               
###and to improve their functionality in R. Where t was previously used it has been
###replaced with time as per the features_info.txt and the f was replaced with freq denoting             
###frequency, acc was replaced with accel indicating acceleration to give the final titles more understandable forms.

19.Assigns names to a varible that will be used to name 
the columns on the post-meaned dataset. The word "mean" 
is affixed to the beginning of each label, but all other 
naming conventions that were previously established are followed.

20.Names the pre-meaned data with cleaned and meaningful names.
21.Creates initial dataframe that will be the framework for the new tidy dataset. It uses column 3 as the seed data.
22.Uses melt() from the "plyr" package to turn the wide form data into long form data.
23.Uses a while loop and tapply to move through all the subset columns and calculate the mean split by activity and subject.
24.Inside the while loop melt and cbind are used to attach each new column to the tidy dataset.
25.At the end of the loop the post-mean names(namespost) is assigned to the meaned dataframe.
26.Uses write.table to write to "tidydata.txt".


There are two commented out lines of code. Just uncomment them to use them. These lines are:

1.write.csv(combsub,"premean.csv") #writes the precalculated file to a csv file for observation.
2.return(datamean) #allows you to put the cleaned datamean dataframe into a console variable for further manipulation.

To read the clean dataset back into R use the following command(assuming the dataset is in your working directory):

data <- read.table("tidydata.txt", header = TRUE)



The full commented code is below:


run_analysis<-function(){
  if(!require(plyr)){#checks to see if plyr package is installed and installs it if not
    install.packages("plyr")
    
  }
  library(plyr)#loads plyr package
  names1<-as.data.frame(read.csv("./UCI HAR Dataset/features.txt", sep="",header=FALSE))#reads original column headings 
  names2<<-names1[,2]#subsets dataframe(df) into vector of names
    x_test<-read.csv("./UCI HAR Dataset/test/x_test.txt",sep="",header=FALSE)#reads in x_test data
    y_test<-read.csv("./UCI HAR Dataset/test/y_test.txt",sep="",header=FALSE)#reads in y_test data(activity ids)
    subject_test<-read.csv("./UCI HAR Dataset/test/subject_test.txt",sep="",header=FALSE)#reads in the subject ids for test data
   
    x_train<-read.csv("./UCI HAR Dataset/train/x_train.txt",sep="",header=FALSE)#reads in x_train data
    y_train<-read.csv("./UCI HAR Dataset/train/y_train.txt",sep="",header=FALSE)#reads in y_train data(activity ids)
    subject_train<-read.csv("./UCI HAR Dataset/train/subject_train.txt",sep="",header=FALSE)#reads in subject ids for train data
 
   colnames(x_test)<-names2#assigns column names (these came from the features.txt file) to test and train sets respectively.
   colnames(x_train)<-names2#these are necessary to get columns dealing with means[mean()] and standard deviations [std()]
 
  y_test_train<-rbind(y_test,y_train)#combine activity values for test and train giving single column dataframe 10299 values long
  
  colnames(y_test_train)<-"activityname"#names activity column for subsetting later
  y_test_train[y_test_train==1]<-"Walking"#replaces activity ids with activity names as detailed in the activity_labels.txt file
  y_test_train[y_test_train==2]<-"Walking Upstairs"
  y_test_train[y_test_train==3]<-"Walking Downstairs"
  y_test_train[y_test_train==4]<-"Sitting"
  y_test_train[y_test_train==5]<-"Standing"
  y_test_train[y_test_train==6]<-"Laying"
  
  subject_test_train<-rbind(subject_test,subject_train)#combine subject identifiers for test and train giving dataframe of a single column and 10299 values long
  colnames(subject_test_train)<-"subjectid"#names the combined subject identifier column for subsetting later
  
  x_test_train<-rbind(x_train,x_test)#combine named dataframes x_test and x_train giving dataframe 10299 values long
    
  names_mean<-x_test_train[,grep("mean",names(x_test_train),value=TRUE, ignore.case=TRUE)]#gets columns with "mean" in the name from combined x_test and x_train datasets
  names_std<-x_test_train[,grep("std",names(x_test_train),value=TRUE, ignore.case=TRUE)]#gets columns with "std" in the name from combined x_test and x_train datasets
  combsub<-as.data.frame(cbind(subject_test_train,y_test_train,names_mean,names_std))#puts the subject and activity identifiers with the subsetted columns
  
  namespre<-c("subjectId","activityName","timeBodyAccel.mean.X",#names for the pre meaned data set. These are provided so that if  
              "timeBodyAccel.mean.Y","timeBodyAccel.mean.Z",    #someone wants to see the premeaned data they can see it.
           "timeGravityAccel.mean.X","timeGravityAccel.mean.Y", #column names used to have - and () in them. These are problematic for R
           "timeGravityAccel.mean.Z","timeBodyAccelJerk.mean.X",#Where these characters existed, they have been replaced with ".".
           "timeBodyAccelJerk.mean.Y"                           #in the sections where there were multiple words strung together,
           ,"timeBodyAccelJerk.mean.Z","timeBodyGyro.mean.X"    #camelCase has been used. These modifications were done to preserve readability               
           ,"timeBodyGyro.mean.Y","timeBodyGyro.mean.Z"         #and to improve their functionality in R. Where t was previously used it has been
           ,"timeBodyGyroJerk.mean.X","timeBodyGyroJerk.mean.Y" #replaced with time as per the features_info.txt and the f was replaced with freq denoting             
           ,"timeBodyGyroJerk.mean.Z","timeBodyAccelMag.mean"   #frequency, acc was replaced with accel to give the final titles more understandable meanings               
           ,"timeGravityAccelMag.mean","timeBodyAccelJerkMag.mean"             
           ,"timeBodyGyroMag.mean","timeBodyGyroJerkMag.mean"             
           ,"freqBodyAccel.mean.X","freqBodyAccel.mean.Y"                   
           ,"freqBodyAccel.mean.Z","freqBodyAccel.meanFreq.X"               
           ,"freqBodyAccel.meanFreq.Y","freqBodyAccel.meanFreq.Z"               
           ,"freqBodyAccelJerk.mean.X","freqBodyAccelJerk.mean.Y"               
           ,"freqBodyAccelJerk.mean.Z","freqBodyAccelJerk.meanFreq.X"           
           ,"freqBodyAccelJerk.meanFreq.Y","freqBodyAccelJerk.meanFreq.Z"           
           ,"freqBodyGyro.mean.X","freqBodyGyro.mean.Y"                  
           ,"freqBodyGyro.mean.Z","freqBodyGyro.meanFreq.X"              
           ,"freqBodyGyro.meanFreq.Y","freqBodyGyro.meanFreq.Z"              
           ,"freqBodyAccelMag.mean","freqBodyAccelMag.meanFreq"              
           ,"freqBodyAccelJerkMag.mean","freqBodyAccelJerkMag.meanFreq"      
           ,"freqBodyGyroMag.mean","freqBodyGyroMag.meanFreq"         
           ,"freqBodyGyroJerkMag.mean","freqBodyGyroJerkMag.meanFreq"     
           ,"angle.timeBodyAccelMean.gravity","angle.timeBodyAccelJerkMeangravityMean"
           ,"angle.timeBodyGyroMean.gravityMean","angle.timeBodyGyroJerkMean.gravityMean"
           ,"angle.X.gravityMean","angle.Y.gravityMean"                
           ,"angle.Z.gravityMean","timeBodyAccel.std.X"                    
           ,"timeBodyAccel.std.Y","timeBodyAccel.std.Z"                    
           ,"timeGravityAccel.std.X","timeGravityAccel.std.Y"                 
           ,"timeGravityAccel.std.Z","timeBodyAccelJerk.std.X"                
           ,"timeBodyAccelJerk.std.Y","timeBodyAccelJerk.std.Z"                
           ,"timeBodyGyro.std.X","timeBodyGyro.std.Y"                   
           ,"timeBodyGyro.std.Z","timeBodyGyroJerk.std.X"               
           ,"timeBodyGyroJerk.std.Y","timeBodyGyroJerk.std.Z"               
           ,"timeBodyAccelMag.std","timeGravityAccelMag.std"                
           ,"timeBodyAccelJerkMag.std","timeBodyGyroMag.std"                  
           ,"timeBodyGyroJerkMag.std","freqBodyAccel.std.X"                    
           ,"freqBodyAccel.std.Y","freqBodyAccel.std.Z"                    
           ,"freqBodyAccelJerk.std.X","freqBodyAccelJerk.std.Y"                
           ,"freqBodyAccelJerk.std.Z","freqBodyGyro.std.X"                   
           ,"freqBodyGyro.std.Y","freqBodyGyro.std.Z"                   
           ,"freqBodyAccelMag.std","freqBodyAccelJerkMag.std"           
           ,"freqBodyGyroMag.std","freqBodyGyroJerkMag.std")
  namespost<-c("activityName","subjectId","meanTimeBodyAccel.mean.X",#names for columns after the means are taken. The previously stated 
               "meanTimeBodyAccel.mean.Y","meanTimeBodyAccel.mean.Z",#conventions are adhered to but mean has been added to every label to 
               "meanTimeGravityAccel.mean.X","meanTimeGravityAccel.mean.Y"#show the calculations have been completed.
               ,"meanTimeGravityAccel.mean.Z","meanTimeBodyAccelJerk.mean.X",
               "meanTimeBodyAccelJerk.mean.Y"               
               ,"meanTimeBodyAccelJerk.mean.Z","meanTimeBodyGyro.mean.X"                  
               ,"meanTimeBodyGyro.mean.Y","meanTimeBodyGyro.mean.Z"                  
               ,"meanTimeBodyGyroJerk.mean.X","meanTimeBodyGyroJerk.mean.Y"              
               ,"meanTimeBodyGyroJerk.mean.Z","meanTimeBodyAccelMag.mean"                  
               ,"meanTimeGravityAccelMag.mean","meanTimeBodyAccelJerkMag.mean"              
               ,"meanTimeBodyGyroMag.mean","meanTimeBodyGyroJerkMag.mean"             
               ,"meanFreqBodyAccel.mean.X","meanFreqBodyAccel.mean.Y"                   
               ,"meanFreqBodyAccel.mean.Z","meanFreqBodyAccel.meanFreq.X"               
               ,"meanFreqBodyAccel.meanFreq.Y","meanFreqBodyAccel.meanFreq.Z"               
               ,"meanFreqBodyAccelJerk.mean.X","meanFreqBodyAccelJerk.mean.Y"               
               ,"meanFreqBodyAccelJerk.mean.Z","meanFreqBodyAccelJerk.meanFreq.X"           
               ,"meanFreqBodyAccelJerk.meanFreq.Y","meanFreqBodyAccelJerk.meanFreq.Z"           
               ,"meanFreqBodyGyro.mean.X","meanFreqBodyGyro.mean.Y"                  
               ,"meanFreqBodyGyro.mean.Z","meanFreqBodyGyro.meanFreq.X"              
               ,"meanFreqBodyGyro.meanFreq.Y","meanFreqBodyGyro.meanFreq.Z"              
               ,"meanFreqBodyAccelMag.mean","meanFreqBodyAccelMag.meanFreq"              
               ,"meanFreqBodyAccelJerkMag.mean","meanFreqBodyAccelJerkMag.meanFreq"      
               ,"meanFreqBodyGyroMag.mean","meanFreqBodyGyroMag.meanFreq"         
               ,"meanFreqBodyGyroJerkMag.mean","meanFreqBodyGyroJerkMag.meanFreq"     
               ,"meanAngle.timeBodyAccelMean.gravity","meanAngle.timeBodyAccelJerkMeangravityMean"
               ,"meanAngle.timeBodyGyroMean.gravityMean","meanAngle.timeBodyGyroJerkMean.gravityMean"
               ,"meanAngle.X.gravityMean","meanAngle.Y.gravityMean"                
               ,"meanAngle.Z.gravityMean","meanTimeBodyAccel.std.X"                    
               ,"meanTimeBodyAccel.std.Y","meanTimeBodyAccel.std.Z"                    
               ,"meanTimeGravityAccel.std.X","meanTimeGravityAccel.std.Y"                 
               ,"meanTimeGravityAccel.std.Z","meanTimeBodyAccelJerk.std.X"                
               ,"meanTimeBodyAccelJerk.std.Y","meanTimeBodyAccelJerk.std.Z"                
               ,"meanTimeBodyGyro.std.X","meanTimeBodyGyro.std.Y"                   
               ,"meanTimeBodyGyro.std.Z","meanTimeBodyGyroJerk.std.X"               
               ,"meanTimeBodyGyroJerk.std.Y","meanTimeBodyGyroJerk.std.Z"               
               ,"meanTimeBodyAccelMag.std","meanTimeGravityAccelMag.std"                
               ,"meanTimeBodyAccelJerkMag.std","meanTimeBodyGyroMag.std"                  
               ,"meanTimeBodyGyroJerkMag.std","meanFreqBodyAccel.std.X"                    
               ,"meanFreqBodyAccel.std.Y","meanFreqBodyAccel.std.Z"                    
               ,"meanFreqBodyAccelJerk.std.X","meanFreqBodyAccelJerk.std.Y"                
               ,"meanFreqBodyAccelJerk.std.Z","meanFreqBodyGyro.std.X"                   
               ,"meanFreqBodyGyro.std.Y","meanFreqBodyGyro.std.Z"                   
               ,"meanFreqBodyAccelMag.std","meanFreqBodyAccelJerkMag.std"           
               ,"meanFreqBodyGyroMag.std","meanFreqBodyGyroJerkMag.std")
  
  colnames(combsub)<-namespre#renames pre calculated data frame with the combined and subset dataframe with cleaned and meaningful names
  #write.csv(combsub,"premean.csv") #uncomment this line if you want to output the cleaned but premeaned dataset to a csv file
  
  datamean<-tapply(combsub[,3],list(combsub$activityName,combsub$subjectId),mean)#creates the initial meaned dataframe
  datamean<-melt(datamean)#puts the data in long form via melt
  
  i<-4#sets the column that will be used as the first in the loop. column 1 is activity ID, column 2 is subject id. column 3 is calculated in the above tapply function
 while(i<=ncol(combsub)){
   
    datamean2<-tapply(combsub[,i],list(combsub$activityName,combsub$subjectId),mean) #creates mean data splitting by activity and subject and ordering it by subject.
    
    datamean2<-melt(datamean2)#puts data in long form
    
    
    datamean<-cbind(datamean,datamean2$value)#extracts the value column from the melted frame and binds it to the datamean frame
   i<-i+1#iterates the counter for the while loop
    
  }
  colnames(datamean)<-namespost#assigns the postnames to the cleaned and meaned dataset
  write.table(datamean,"tidydata.txt",row.name=FALSE) 
  #return(datamean)#uncomment this line if you want to output the tidy data to the console.
}