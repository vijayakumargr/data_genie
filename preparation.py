library(plyr)
library(dplyr)
library(jsonlite)
library(purrr)
library(tidyr)
library(tidyverse)

#Change the below path to point to your files
FolderName = "~/Desktop/Project Zip/"

zipFiles = list.files(FolderName)
zips=length(zipFiles)
master = list()

#Function to Parse JSON Columns in the data
ParseJSONColumn <- function(x)  {
  str_c("[ ", str_c(x, collapse = ",", sep=" "), " ]")  %>% 
    fromJSON(flatten = T) %>% 
    as.tibble()
}

#Function to combine unmatching columns in the dataframe - will include columns from both dataframes
df_column_match <- function(x, y) {
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  x[, c(as.character(y.diff))] <- NA
  y[, c(as.character(x.diff))] <- NA
  return(rbind(x, y))
}

#Loop through Zip file, unzip them , loop through the csv files and combine into one dataset

for (i in 1:zips) {
  print(zipFiles[i])
  master[[i]] = as.character(unzip(paste(FolderName,zipFiles[i],sep = ""),
                                   list = TRUE)$Name)
  data=vector(mode = "list")
  combineddf=data_frame()
  numberofcsv = length(master[[i]])
  for (j in 1:numberofcsv) {
    print(master[[i]][j])
    data[[j]] = read.csv(unz(paste(FolderName,zipFiles[i],sep = ""), master[[i]][j]), header = TRUE,
                         sep = ",")
  }
  tmpdf=ldply(data, rbind)
  export_date=str_replace(zipFiles[i],".zip","")
  if(i==1){
    datadf_all=rbind(tmpdf)
    datadf_all$scrape_date=export_date
  }
  else{
    datadf = rbind(tmpdf)
    datadf$scrape_date=export_date
    datadf_all=df_column_match(datadf_all, datadf)
  }
}
datadf_all$row_num <- seq.int(nrow(datadf_all)) 

#change Blanks to NA
datadf_all[datadf_all==""]<-NA


#Flatten Category JSON Data
category_data <- datadf_all  %>% 
  select(row_num,category)  %>% 
  map_dfc(.f = ParseJSONColumn)

#Flatten Location JSON Data
location_data <- datadf_all  %>% 
  filter(is.na(location)==FALSE)
select(row_num,location)  %>% 
  map_dfc(.f = ParseJSONColumn)

#Remove unwanted columns
datadf_all = subset(datadf_all, select = -c(unseen_activity_count,country_displayable_name,last_update_published_at,source_url) )

#Join Location Data and add columns to main dataframe
datadf_all$state_code <- location_data$state[match(datadf_all$row_num, location_data$value)]
datadf_all$city_code <- location_data$localized_name[match(datadf_all$row_num, location_data$value)]
datadf_all$location_id <- location_data$id[match(datadf_all$row_num, location_data$value)]
datadf_all$location_country <- location_data$country[match(datadf_all$row_num, location_data$value)]

#Join Category Data and add columns to main dataframe
datadf_all$category_id <- category_data$id[match(datadf_all$row_num, category_data$value)]
datadf_all$category_name <- category_data$name[match(datadf_all$row_num, category_data$value)]
datadf_all$category_slug <- category_data$slug[match(datadf_all$row_num, category_data$value)]
datadf_all$category_position <- category_data$position[match(datadf_all$row_num, category_data$value)]

#Date columns fix 
datadf_all$created_at = as.Date(as.POSIXct(datadf_all$created_at, origin="1970-01-01"))
datadf_all$deadline = as.Date(as.POSIXct(datadf_all$deadline, origin="1970-01-01"))
datadf_all$launched_at = as.Date(as.POSIXct(datadf_all$launched_at, origin="1970-01-01"))
datadf_all$state_changed_at = as.Date(as.POSIXct(datadf_all$state_changed_at, origin="1970-01-01"))

count(datadf_all)
write.csv(datadf_all, paste(FolderName,"kickstarter.csv"))
print("done")
warnings()
