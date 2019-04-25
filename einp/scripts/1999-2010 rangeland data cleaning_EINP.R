# task: 1999-2010 data cleaning einp
# author: ricky weng

# library
library(tidyverse) # dplyr
library(readxl) # read and write
library(purrr) # function

# set working directory and read csv data file 
setwd("U:/EINP data")

# define read_then_csv
read_then_csv <- function (sheet, path) {
  path%>%
    read_excel(sheet = sheet) %>% 
    write_csv(paste0(sheet, ".csv"))
}
# Iterate excel sheets and save as csv
path <- "Rangeland Reference 1999-2010 data Osko_2011-03-29.xls"
path%>%
  excel_sheets()%>%
  set_names()%>% 
  map(read_then_csv, path = path)

# add description of site data sheet
# create a new data frame 
df <- read.csv("Site Data.csv", sep = ",", header = FALSE)
df1 <- df[c(2:13), ]
write.table(df1, "Site Data.csv", sep = ",", row.names = FALSE, col.names = FALSE, na = "")
df2 <- data.frame("Category" = df[c(19:30, 32), 3], "Description" = df[c(19:30, 32), 5])
write_csv(df2, "Description of Site Data.csv", na = "")

# change Data Key sheet to readable format
df <- read.csv("Data Key.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
df <- data.frame("Species" = c(df[9:19, 1], df[9:15, 3], df[9:14, 5], df[22:26, 1], df[22:24, 4]),
              "Category" = c(rep(df[8, 1], 11), rep(df[8, 3], 7), rep(df[8, 5], 6), rep(df[21, 1], 5), rep(df[21, 4], 3)))
write.table(df,"Data Key.csv", sep = ",", row.names = FALSE, na = "")

# format multiple csv files
file_list <- list.files(pattern = "*.csv")

for (i in seq_along(file_list)) {
  filename <- file_list[[i]]
  # read data in
  df <- read.table(filename, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  # change "." to blank
  df[df == "."] <- ""
  # change Site, Year, Graze, Aspect numbers to names.
  df <- df%>%
    mutate_at(vars(contains("Site")), funs(ifelse(.== 1, "Ranger",
                                            ifelse(.==2, "Boreal",
                                                   ifelse(.==3, "Beaver",
                                                          ifelse(.==4, "Shirley",
                                                                 ifelse(.==5, "Tawayik",
                                                                        ifelse(.==6, "BAILEY", ""))))))))%>%
    mutate_at(vars(contains("Year")),funs(ifelse(.==1, 1999,
                                                 ifelse(.==2, 2000,
                                                        ifelse(.==5, 2003,
                                                               ifelse(.==11, 2010, ""))))))%>%
    mutate_at(vars(contains("Graz")),funs(ifelse(.==1, "Inside",
                                               ifelse(.==2, "Outside", ""))))%>%
    mutate_at(vars(contains("Aspect")),funs(ifelse(.==1, "North",
                                              ifelse(.==2, "South", ""))))
  # export cleaned csv files  
  write.table(df, filename, sep = ",", row.names = FALSE, na = "")
}  

# combine to one workbook
for (i in seq_along(file_list)) {
  filename <- file_list[[i]]
  # Read data in
  df <- read.table(filename, sep = ",", header = FALSE, fill = TRUE, stringsAsFactors = FALSE, na.strings = "")
  write.xlsx(df, file = "Rangeland Reference 1994-2010 data Osko_2019-01-24.xlsx", sheetName = gsub(filename, pattern = ".csv", replacement = ""), append = TRUE)
}





