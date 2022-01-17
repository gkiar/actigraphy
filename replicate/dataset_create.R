### Generating Datasets for HBN actigraphy data release from GGIR output

### Import 60s epoch ENMO counts and non-wear score from part 1 

### Create long format dataset in 60s and 600s epoch
### Create day level nonwear summaries
### Create filtered dataset, at least 3 days of 95% wear at 60s and 600s epoch
### Create common sleep variables dataset from part5 day level summary

### Create day level filter dataset

### Filter to create clean dataset

#REMEMBER TO USE REMOVE DUPLICATE SCRIPT TO GET RID OF:
#1) The duplicated raw.csv files
#2) The duplicated meta rdata files
#3) The duplicated .gt3x files

#AFTER REMOVING DUPLICATES, REPLACE GUID

#GGIR RUN NO INPUTE

#save project root directory
#project.root = getwd()

#load packages
library(tidyverse)

#load functions into environment
source('/home/Gregory.Kiar/code/gkiar/actigraphy/replicate/functions.R')

#get filenames to load in data
filenames <- list.files('/data/actigraphy/HBN/raw/myggir/output_csv/meta/basic')

#clean file name to only leave ID 
ID <- str_replace_all(filenames, c('meta_' = '', '.csv.RData' = ''))

#load in and clean data from all subjects, then combine into long-form dataset

setwd('/data/actigraphy/HBN/raw/myggir/output_csv/meta/basic')

for (i in c(1:length(filenames))) {
  
  #load data
  load(filenames[i])
  subjectData <- M$metashort
  subjectNonwear <- M$metalong
  
  #reformat 5s GGIR output and summarize into 60s epoch
  subjectData <- reformat_data(subjectData, ID[i])
  
  #reformat nonwear score data (900s epoch)
  subjectNonwear <- reformat_nonwear(subjectNonwear)
  
  #join nonwear score to 60s ENMO dataset
  subjectData <- left_join(subjectData, subjectNonwear, by = c('day', 'minute'))
  rm(subjectNonwear)
  
  #nonwearscore carried forward (code from Dr. Wei Guo from NIMH)
  index <- which(!is.na(subjectData$nonwearscore))
  
  for (j in 1:length(index)) {
    
    if (j != length(index)) {
      
      subjectData$nonwearscore[(index[j] + 1):(index[j+1]) - 1] <- subjectData$nonwearscore[(index[j])]
    
      }
    
    else {
      
      subjectData$nonwearscore[(index[j] + 1):nrow(subjectData)] <- subjectData$nonwearscore[(index[j])]
    
      }
  
  }
  
  
  

  #append dataset
  
  if (i==1) {
    
    allData <- subjectData
    
  }
  
  else {
    
    allData <- rbind(allData, subjectData)
    
  }
  
  message(paste0("...file ", i, ": ", filenames[i], " completed"))
  
  rm(list = c('C', 'I', 'M', 'subjectData'))

}

#recode nonwear, if nonwearscore = 0 or 1, wear = 1
#if nonwearscore = 2 or 3, wear = 0
allData <- allData %>% mutate(wear = ifelse(nonwearscore < 2, 1, 0))
allData <- allData %>% select(-nonwearscore)

#export this dataset
write_csv(allData, '/data/actigraphy/HBN/raw/processed_data/epoch/true_timestamps/HBN_actigraphy_60s.csv')

#downsample this data for plotting purposes and export
allData10 <- downsample(allData, 10)

#export version without timestamps
allData_nt <- mask_timestamp(allData)
allData10_nt <- mask_timestamp(allData10)

write_csv(allData_nt, '/data/actigraphy/HBN/raw/processed_data/epoch/no_timestamps/HBN_actigraphy_60s.csv')
write_csv(allData10_nt, '/data/actigraphy/HBN/raw/processed_data/epoch/no_timestamps/HBN_actigraphy_600s.csv')

#export downsampled dataset
write_csv(allData10, '/data/actigraphy/HBN/raw/processed_data/epoch/true_timestamps/HBN_actigraphy_600s.csv')

#create valid wear summary dataset for filtering 
validwear <- allData %>%
  group_by(ID, day) %>%
  summarise(minute_wear = sum(wear, na.rm = T))

#export validwear dataset
write_csv(validwear, '/data/actigraphy/HBN/processed_data/epoch/valid_wear.csv')

#filter data by at criteria of least 3 days of >95% valid weardata$timestamp <- format(data$timestamp, tz = "Etc/GMT+5")

#find out why timestamps are getting coerced to character
allData$timestamp <- as.POSIXct(allData$timestamp, tz = "America/New_York")
filteredData <- filter_data(allData, validwear, 0.95, 3)

#inpute nonwear
filteredData <- impute_nonwear(filteredData)

#downsample to 600s epoch
filteredData10 <- downsample(filteredData, 10)

#drop 'wear' column from cleaned datasets
filteredData <- filteredData %>% select(-wear)
filteredData10 <- filteredData10 %>% select(-wear)
#export cleaned dataset 60s epoch dataset

write_csv(filteredData, '/data/actigraphy/HBN/raw/processed_data/epoch/true_timestamps/HBN_actigraphy_60s_95wear_3day.csv')

#export cleaned 600s epoch dataset
write_csv(filteredData10, '/data/actigraphy/HBN/raw/processed_data/epoch/true_timestamps/HBN_actigraphy_600s_95wear_3day.csv')


#export version without timestamps
filteredData_nt <- mask_timestamp(filteredData)
filteredData10_nt <- mask_timestamp(filteredData10)

write_csv(filteredData_nt, '/data/actigraphy/HBN/raw/processed_data/epoch/no_timestamps/HBN_actigraphy_60s_95wear_3day.csv')
write_csv(filteredData10_nt, '/data/actigraphy/HBN/raw/processed_data/epoch/no_timestamps/HBN_actigraphy_600s_95wear_3day.csv')







