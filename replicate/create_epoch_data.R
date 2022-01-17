#create 60s epoch data from part1 GGIR output

#include nonwear scores

#export version with and without timestamps into csv
library(tidyverse)

#source functions
source('/data2/HBNcore/CMI_HBN_Data/actigraphy/scripts/dataset_create/functions.R')

#get subject ID
meta_files <- list.files('/data2/HBNcore/CMI_HBN_Data/actigraphy/GGIR/output_csv/meta/basic')
ID <- str_replace_all(meta_files, c('meta_' = '', '.csv.RData' = ''))

setwd('/data2/HBNcore/CMI_HBN_Data/actigraphy/GGIR/output_csv/meta/basic')

for (i in c(1:length(meta_files))) {

  #load data
  load(meta_files[[i]])

  subjectData <- M$metashort
  subjectNonwear <- M$metalong

  #reformat 5s GGIR output and summarize into 60s epoch
  subjectData <- reformat_data(subjectData, ID[[i]])

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
  
  message(paste0("...file ", i, ": ", meta_files[[i]], " completed"))
  
  rm(list = c('C', 'I', 'M', 'subjectData'))
  
}

#recode nonwear, if nonwearscore = 0, wear = 1
#if nonwearscore = 1, 2, or 3, wear = 0
allData <- allData %>% mutate(wear = ifelse(nonwearscore < 1, 1, 0))
allData <- allData %>% select(-nonwearscore)

#export this dataset
write_csv(allData, '/data2/HBNcore/CMI_HBN_Data/actigraphy/processed_data/epoch/aggregate/HBN_actigraphy_60s.csv')

#downsample this data for plotting purposes and export
allData10 <- downsample(allData, 10)

#export downsampled dataset
write_csv(allData10, '/data2/HBNcore/CMI_HBN_Data/actigraphy/processed_data/epoch/aggregate/HBN_actigraphy_600s.csv')

#create valid wear summary dataset for filtering 
validwear <- allData %>%
  group_by(ID, day) %>%
  summarise(minute_wear = sum(wear, na.rm = T))

#export validwear dataset
write_csv(validwear, '/data2/HBNcore/CMI_HBN_Data/actigraphy/processed_data/epoch/aggregate/valid_wear.csv')

#filter data by at criteria of least 3 days of >95% valid wear 
filteredData <- filter_data(allData, validwear, 0.95, 3)

#inpute nonwear
filteredData <- impute_nonwear(filteredData)

#downsample to 600s epoch
filteredData10 <- downsample(filteredData, 10)

#drop 'wear' column from cleaned datasets
filteredData <- filteredData %>% select(-wear)
filteredData10 <- filteredData10 %>% select(-wear)

#export cleaned dataset 60s epoch dataset
write_csv(filteredData, '/data2/HBNcore/CMI_HBN_Data/actigraphy/processed_data/epoch/aggregate/HBN_actigraphy_60s_95wear_3day.csv')

#export cleaned 600s epoch dataset
write_csv(filteredData10, '/data2/HBNcore/CMI_HBN_Data/actigraphy/processed_data/epoch/aggregate/HBN_actigraphy_600s_95wear_3day.csv')






