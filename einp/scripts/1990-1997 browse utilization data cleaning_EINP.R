# task: 1990-1997 browse utilization data cleaning einp
# author: ricky weng

# library
library(tidyverse) # data cleaning
library(xlsx) # export as a workbook

# set workspace 
setwd("U:/EINP data/Browse Utilization 1990-1997/Data") 

# read csv file and edit 1996 and 1997 data to make them consistent with others
df <- read.csv("1996.csv", sep = ",", header = TRUE, check.names = FALSE, na.strings = "")
names(df)[c(5, 6)] <- c("Browsed#", "Unbrowsed#")
write.csv(df, "1996.csv", na = "", row.names = FALSE)
df <- read.csv("1997.csv", sep = ",", header = TRUE, check.names = FALSE, na.strings = "")
df <- df[-c(3855, 3856), ]
names(df)[c(5, 6, 7, 9, 10)] = c("Browsed#", "Unbrowsed#", "Dead", "Browsed%", "Unbrowsed%")
write.csv(df, "1997.csv", na = "", row.names = FALSE)

# add year data and combine 1990-1997 original data to one sheet
file_list <- list.files(pattern = "19.*csv")
file_list
# get all data frames of 1990-1997 data
All <- lapply(file_list, function(i) {
  df <- read.csv(i, sep = ",", header = TRUE, check.names = FALSE)
  df <- df%>%
    mutate(Year = gsub(i, pattern = ".csv", replacement = ""))
  assign(i, df)
})
# combine to one 
originaldata <- do.call(rbind.data.frame, All)

# clean By_trans data 
df <- read.csv("By_trans.csv", sep = ",", header = FALSE, check.names = FALSE, na.strings = "")
# change header names
colnames(df) = as.character(unlist(df[3, ]))
# remove useless rows and column 
i <- seq(0, 49, 7)
df <- df[-c(i, i+1, i+2, i+3), ]
df <- df[ , -8]
# add years
df <- df%>%
  mutate(Year = rep(1990 : 1997, each = 3))
df[24, 1] = "Average of Browsed%"

By_trans <- df%>%
  gather(key = "Transect", value = "Value" , 2 : 7)%>%
  spread(Data, Value)

## clean By_spp data 
df <- read.csv("By_spp.csv", sep = ",", header = FALSE, check.names = FALSE, na.strings = c("", "#DIV/0!"))
df <- distinct(df)
# get and clean 1990, 1993 and 1994 data
year909394 <- df%>%
  .[c(3 : 6, 19 : 21, 23 : 25), ]%>%
  .[ ,-c(19, 20)]
# change header names and add years
colnames(year909394) <- as.character(unlist(year909394[1, ]))  
year909394 <- year909394%>%
  .[-1, ]%>%
  mutate(Year = rep(c(1990, 1993, 1994), each = 3))%>%
  # transform to long format 
  gather(key = "Species", value = "Value" , 2 : 18)%>%
  spread(Data, Value)

# get and clean 1991 data
year91 <- df%>%
  .[c(9 : 12), ]%>%
  .[ , -20]
# change header names and add years
colnames(year91) <- as.character(unlist(year91[1, ])) 
year91 <- year91%>%
  .[-1, ]%>%
  mutate(Year = rep(1991, each = 3))%>%
  # transform to long format 
  gather(key = "Species", value = "Value", 2 : 19)%>%
  spread(Data, Value)
  
# get and clean 1992, 1995, 1996 and 1997 data
year92959697 <- df%>%
  .[c(14 : 17, 27 : 29, 31 : 33, 35 : 37), ]%>%
  .[ , -c(18 : 20)]
# change header names and add years
colnames(year92959697) <- as.character(unlist(year92959697[1, ])) 
year92959697 <- year92959697%>%
  .[-1, ]%>%
  mutate(Year = rep(c(1992, 1995 : 1997), each = 3))
year92959697[12, 1] <- "Average of Browsed%"
# transform to long format 
year92959697 <- year92959697%>%
  gather(key = "Species", value = "Value", 2 : 17)%>%
  spread(Data, Value)

By_spp <- bind_rows(year909394, year91, year92959697)

## clean Spp_comp data 
df <- read.csv("Spp_comp.csv", sep = ",", header = FALSE, check.names = FALSE, na.strings = "")
# get data and change header names
Spp_comp <- df%>%
  .[c(10:27, 33:49, 55:70, 76:92, 98:114, 120:135, 141:156, 162:177), -c(8:15)]
colnames(Spp_comp) <- as.character(unlist(Spp_comp[1, ]))
# add years and transform to long format
Spp_comp <- Spp_comp%>%
  .[-1, ]%>%
  mutate(Year = rep(c(1990:1997), c(17, 17, 16, 17, 17, 16, 16, 16)))%>%
  gather(key = "Transect", value = "Count of Total Twigs", 2:7)
# get compostion data and add years
Spp_comp_byspecies <- df%>%
  .[ ,-c(2:7, 14, 15)]%>%
  .[-c(1:10, 28:32, 50:54, 71:75, 93:97, 115:119, 136:140, 157:161, 178), ]%>%
  mutate(Year = rep(c(1990:1997), c(17, 17, 16, 17, 17, 16, 16, 16)))
names(Spp_comp_byspecies) <- c("Species", "Grand Total Twigs", "%Composition", "#Isolation", "%Isolation", "#Main", "%Main", "Year")

## export all data frames to a workbook called Browse Utilization 1990-1997
write.xlsx(originaldata, file = "Browse Utilization 1990-1997.xlsx", 
           sheetName = "Original data", append = TRUE)
write.xlsx(By_trans, file = "Browse Utilization 1990-1997.xlsx", 
           sheetName = "By_trans", append = TRUE)
write.xlsx(By_spp,file = "Browse Utilization 1990-1997.xlsx", 
           sheetName = "By_spp", append = TRUE)
write.xlsx(Spp_comp,file="Browse Utilization 1990-1997.xlsx", 
           sheetName = "Spp_comp", append = TRUE)
write.xlsx(Spp_comp_byspecies, file = "Browse Utilization 1990-1997.xlsx", 
           sheetName = "Spp_comp_byspecies", append = TRUE)

