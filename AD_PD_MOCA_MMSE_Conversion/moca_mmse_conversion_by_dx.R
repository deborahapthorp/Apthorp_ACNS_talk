#This script is used to convert MMSE to MoCA scores and/or MoCA scores to MMSE scores. 
#These conversions are computed based upon diagnosis as there are published conversion tables for  Alzheimer's diseas and Parkinson's (PD) 
#See Roalf et al., 2013 Alz. & Dementia and van Steenoven et al. 2014 Movement Disorders
#Missing data should be noted by either a ".", "NA" in the cell, or by leaving the cell blank.
#In order for this script to work your data will need to include the following columns "MMSE", "MoCA", and "dx" (which gets diagnosis either 
#PD, AD, Other (if dx is missing then the scores won't be converted)). The 'Other' group includes healthy individuals and the AD version of the 
#imputation is used since MoCA 2 MMSE conversion was completed on a larger more distributed sample. In addition you will also need the 
#four conversion table csv's listed below in the "Import Data and Functions" section

#######################################
###Import Data and Specify Variables###
#######################################

#change the "path" variable below to the path of your data file, the conversion tables, and where you wish the converted data to be saved out
path<- "/Users/deborahapthorp/Dropbox (Personal)/Conferences/ACNS2025/Apthorp_ACNS_talk/AD_PD_MOCA_MMSE_Conversion"

path2<- "/Users/deborahapthorp/Dropbox (Personal)/Conferences/ACNS2025/Apthorp_ACNS_talk/docs/data"

#read in your data; Change "Test.csv" to the name of your data file
orig_data<- read.csv(paste(path2,"UTU_wide.csv",sep="/"),na.strings=c(".","","NA"))

#read in the tables with the conversions
pd_mmse_to_moca<- read.csv(paste(path,"PD_MMSE_2_MoCA.csv",sep="/"))
pd_moca_to_mmse<- read.csv(paste(path,"PD_MoCA_2_MMSE.csv",sep="/"))
ad_mmse_to_moca<- read.csv(paste(path,"AD_MMSE_2_MoCA.csv",sep="/"))
ad_moca_to_mmse<- read.csv(paste(path,"AD_MoCA_2_MMSE.csv",sep="/"))

#As some scores translate to more than one score on the other test certain scores can be a decimal. 
#You can choose to to round or not round the converted MoCA scores up or down to the next whole number. 
#The default is NO rounding (rounding<- "none").
#If you wish to round up, change the function below to rounding<- "up", 
#if you wish to round down change the function below to rounding<- "down"
rounding<- "none"

#create a variable called "date" that get's today's date (i.e. the date you run the script) in year_month_day format. 
#This variable will be used as an appendix to the filename of your converted data when it's saved out at the end
date<- as.character(format(Sys.Date(), "%Y_%m_%d"))

###############
###Data Prep###
###############

#make a new dataframe called "data". This is a opy of your orignial data. All work is done on this new dataframe. Original data
#should be untouched. 
data<- orig_data

#make sure your MoCA and MMSE score columns are numeric
data$MoCA<- as.numeric(as.character(data$MoCA))
data$MMSE<- as.numeric(as.character(data$MMSE))

#make a notes column called "conversion notes" to catch any errors in data (i.e. out of range scores (e.g. MoCA=40), or missing 'dx')
data$conversion_notes<- NA

##################
###Conversions####
##################

#loop through all rows in data
for (i in 1:nrow(data)){
  
  #if the MoCA or MMSE score is invalid (i.e. a MoCA score of 50, etc.) then put a note in the conversion notes column and move
  #to the next row
  if (isTRUE(data$MoCA[i]>30) | isTRUE(data$MMSE[i]>30) | isTRUE(data$MoCA[i]<0) | isTRUE(data$MMSE[i]<0)){
    
    data$conversion_notes[i]<- "ERROR MoCA or MMSE score above 30 or less than 0"
    next
    
  } #end error check if statement
  
  
  ####For missing diagnoses (if there is a missing diagnosis, go to the next line)
  
  if (isTRUE(is.na(data$dx[i]))){
    
    data$conversion_notes[i]<- "ERROR diagnosis missing"
    next
    
    
    ####For PD diagnoses
  } else if (data$dx[i]=="PD"){
    
    #if both MoCA AND MMSE are missing, move to the next line as you can't convert the scores
    if (is.na(data$MoCA[i] & is.na(data$MMSE[i]))){
      next
      
      #convert missing MoCA scores using existing MMSE scores 
      
    } else if (is.na(data$MoCA[i])){
      
      #make a variable which gets the MMSE score so can find the correct column in the conversion table
      MMSE_score<- as.numeric(data$MMSE[i])
      
      #replace the missing MoCA score with the converted MMSE score from conversion table
      data$MoCA[i]<- pd_mmse_to_moca$MoCA[pd_mmse_to_moca$MMSE==MMSE_score]
      
      #convert missing MMSE scores using existing MoCA scores 
      
    } else if (is.na(data$MMSE[i])){
      
      #make a variable which gets the MoCA score so can find the correct column in the conversion table
      MoCA_score<- as.numeric(data$MoCA[i])
      
      #replace the missing MMSE score with the converted MMSE score from conversion table
      data$MMSE[i]<- pd_moca_to_mmse$MMSE[pd_moca_to_mmse$MoCA==MoCA_score]
      
    } #end the if statements finding missing MoCA or MMSe (if neither missing, if MoCA missing, if MMSE missing)
    
    
    ####For AD diagnoses
    
  } else if (data$dx[i]=="AD" | data$dx[i]=="Other"){
    
    #if both MoCA AND MMSE are missing, move to the next line as you can't convert the scores
    if (is.na(data$MoCA[i] & is.na(data$MMSE[i]))){
      
      next
      
      #convert missing MoCA scores using existing MMSE scores 
      
    } else if (is.na(data$MoCA[i])){
      
      #make a variable which gets the MMSE score so can find the correct column in the conversion table
      MMSE_score<- as.numeric(data$MMSE[i])
      
      #replace the missing MoCA score with the converted MMSE score from conversion table
      data$MoCA[i]<- ad_mmse_to_moca$MoCA[ad_mmse_to_moca$MMSE==MMSE_score]
      
      #convert missing MMSE scores using existing MoCA scores 
      
    } else if (is.na(data$MMSE[i])){
      
      #make a variable which gets the MoCA score so can find the correct column in the conversion table
      MoCA_score<- as.numeric(data$MoCA[i])
      
      #replace the missing MMSE score with the converted MMSE score from conversion table
      data$MMSE[i]<- ad_moca_to_mmse$MMSE[ad_moca_to_mmse$MoCA==MoCA_score]
      
    } #end the if statements finding missing MoCA or MMSe (if neither missing, if MoCA missing, if MMSE missing)
    
    
  } #end conversion diagnosis if statement (if dx = PD, AD, or NA)
  
  
  ########################
  ###Round MoCA Scores####
  ########################
  
  #don't round or round the converted MoCA scores either up or down to the nearest whole number depending what you specify above
  
  if (rounding=="none"){
    
    next  
    
  } else if (rounding=="up"){
    
    #round up
    data$MoCA[i]<- ceiling(data$MoCA[i]) 
    
  } else if (rounding=="down"){
    
    #round down
    data$MoCA[i]<- floor(data$MoCA[i]) 
    
  } #end rounding if statement
  
  
} #end for loop, looping through rows in data (for (i in 1:nrow(data)){)

#####################
###Write out Data####
#####################

#Write new output. Data will be written to path sepcificed above. File will be a .csv file names "converted_data" with the date appended.
#of the file name
write.csv(data,file=paste(path,"/converted_data_",date,".csv",sep=""))


#Created by Megan Quarmley & David Roalf (Updated 04/05/2017)
