#functions 

#1) reformat_data: reformat acceleration data
reformat_data <- function(data, names) {
  
  #create ID from filename
  data <- mutate(data, ID = names)
  
  #create date and time columns
  data$timestamp <- format(data$timestamp, tz = "America/New_York")
  data$Date <- substr(data$timestamp,1,10)
  data$Min <- substr(data$timestamp,12,19)
  ###transform the time format
  data$minute <- map_dbl(strsplit(data$Min,":"),
                         function(x) {
                           x <- as.numeric(x)
                           x[1]*60 + x[2] + 1
                         }
  )
  
  data$day<-order(unique(data$Date))[match(data$Date,unique(data$Date))]
  
  data <- data %>% select(-Date, -Min)
  
  #calculate mean ENMO for 60 second epochs
  data <- data %>% 
    group_by(ID, day, minute) %>%
    summarise(
      timestamp = timestamp[1],
      ENMO = mean(ENMO, na.rm = T))
  
  #simplify timestamp
  data <- data %>% 
    mutate(timestamp = paste0(substr(timestamp, 1, 10), " ", substr(timestamp, 12, 16)))
  
  #reorder columns 
  data <- data[, c(1, 4, 2, 3, 5)]
  
  return(data)
  
}

#2) reformat_nonwear: reformat nonwear data
reformat_nonwear <- function(data) {
  
  
  #create date and time columns
  data$timestamp <- format(data$timestamp, tz = "America/New_York")
  data$Date <- substr(data$timestamp,1,10)
  data$Min <- substr(data$timestamp,12,19)
  ###transform the time format
  data$minute <- map_dbl(strsplit(data$Min,":"),
                         function(x) {
                           x <- as.numeric(x)
                           x[1]*60 + x[2] + 1
                         }
  )
  
  data$day<-order(unique(data$Date))[match(data$Date,unique(data$Date))]
  
  data <- data %>% select(minute, day, nonwearscore)
  
  return(data)
  
}

#3) filter_data: filter data based on %valid minutes per day threshold and minimum number of days that pass this threshold

filter_data <- function(acc_data, wear_data, percent_wear, minimum_days) {
  
  wearSummary <- wear_data %>%
    group_by(ID) %>%
    summarise(day = length(day),
              wear = mean(minute_wear))
  
  #number of subjects in the original dataset
  print(paste0('Input data includes ', length(unique(wearSummary$ID)), ' subjects, ', 
         'averaging ', round(mean(wearSummary$day), 2), ' days per subject and ',
         round(mean(wearSummary$wear), 2), ' minutes of valid wear per day.'))
  
  #keep only days that have greater than percent_wear threshold
  wear_data <- wear_data  %>%
    filter(minute_wear > percent_wear * 1440)
  
  wearSummary <- wear_data %>%
    group_by(ID) %>%
    summarise(day = length(day),
              wear = mean(minute_wear))
  
  #number of subjects in the dataset after filtering by minutes per day
  print(paste0('Data that passed [>', percent_wear*100, '% valid wear per day] threshold includes ', length(unique(wearSummary$ID)), ' subjects, ', 
         'averaging ', round(mean(wearSummary$day), 2), ' days per subject and ',
         round(mean(wearSummary$wear), 2), ' minutes of valid wear per day.'))
  
  #keep only subjects with greater than minimum_days days of data after applying percent_wear threshold
  validIDs <- filter(wearSummary, day > minimum_days)
  validIDs <- unique(validIDs$ID)
  
  wear_data <- wear_data %>% filter(ID %in% validIDs)
  
  wearSummary <- wear_data %>%
    group_by(ID) %>%
    summarise(day = length(day),
              wear = mean(minute_wear))
  
  #number of subjects in the dataset after filtering by minutes per day
  print(paste0('Data that passed [>', minimum_days, ' days] of [>', percent_wear*100, '% valid wear per day] threshold includes ', length(unique(wearSummary$ID)), ' subjects, ', 
         'averaging ', round(mean(wearSummary$day), 2), ' days per subject and ',
         round(mean(wearSummary$wear), 2), ' minutes of valid wear per day.'))
  
  
  validID_days <- paste0(wear_data$ID, '-', wear_data$day)
  
  filtered_data <- acc_data %>% 
    mutate(ID_days = paste0(ID, '-', day)) %>%
    filter(ID_days %in% validID_days) %>%
    select(-ID_days)
  
  return(filtered_data)
  
}


#3) complete_final_day: complete the last recording day to 1440 minutes for each subject

complete_final_day <- function(dataset, IDset) {

  dataset <- dataset %>% filter(ID == IDset)

  n_rows2append <- 1440 - dataset[nrow(dataset),]$minute
  lastday <- dataset[nrow(dataset),]$day

  if (n_rows2append > 0) {
  
    startmin <- 1440 - n_rows2append
    startts <- dataset[nrow(dataset),]$timestamp
    rows2append <- dataset[1:n_rows2append,]
    rows2append <- rows2append %>%
      mutate(minute = c((startmin + 1):1440),
             day = lastday,
             wear = 0,
             ENMO = NA,
             timestamp = seq(startts + 60, startts + n_rows2append*60, by = 60))
  		        
  	dataset <- rbind(dataset, rows2append)
  
  }
  
  return(dataset)

}

#4) impute_nonwear: for minutes with wear = 0, replace ENMO with average ENMO of that minute for each subject 
#NOTE: for 60s epoch data only

impute_nonwear <- function(dataset) {

  #create dataset with minute averages for each participant
  dailyAverage <- dataset %>% 
    group_by(ID, minute) %>%
    dplyr::summarize(avg_ENMO = mean(ENMO, na.rm = T))

  #replace minutes where wear=0 with average minute value for each subject

  #check mean ENMO for nonwear minutes
  before <- dataset %>% filter(wear == 0) %>% ungroup() %>% summarise(mean = mean(ENMO))
  print(paste0('Mean of nonwear minutes BEFORE imputation: ', before$mean))
  
  #complete the last recording day to 1440 minutes for each subject
  dataset <- do.call("rbind", map(unique(dataset$ID), ~complete_final_day(dataset, .)))

  dataset <- dataset %>%
    left_join(dailyAverage, by = c('ID', 'minute')) %>%
    mutate(ENMO = ifelse(wear == 0, avg_ENMO, ENMO)) %>%
  select(-avg_ENMO)

  #check mean ENMO for nonwear minutes after imputation
  after <- dataset %>% filter(wear == 0) %>% ungroup() %>% summarise(mean = mean(ENMO))
  print(paste0('Mean of nonwear minutes AFTER imputation: ', after$mean))
  
  return(dataset)

}

#5) downsample: downsample minute epoch dataset into longer epochs by taking the mean over each epoch

downsample <- function(dataset, epoch) {
  
  print(paste0('Downsampling 60s epoch data into ', epoch, ' minute epochs.'))
  
  dataset <- dataset %>% 
    mutate(
      minute = minute - 1,
      minute = floor(minute/epoch)) %>%
    group_by(ID, day, minute) %>%
    summarise(timestamp = timestamp[1],
              ENMO = mean(ENMO, na.rm = T),
              wear = min(wear)) %>%
    mutate(minute = (minute + 1) * epoch)
  
  return(dataset)
  
}

#6 mask_timestamp: turn timestamp column into day of week and month

mask_timestamp <- function(dataset) {
  
  dataset <- dataset %>%
    mutate(weekday = lubridate::wday(timestamp, label = T, abbr = T),
           month = lubridate::month(timestamp, label = T, abbr = T)) %>%
    select(-timestamp)
  
  return(dataset)
}

