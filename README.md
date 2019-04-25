# Collection of data wrangling, analysis, and visualization work at PCA
It should be noted that raw data, statistical analysis results, and reports are not provided here for security concerns.    
Only selected figures are presented. More figures and scripts can be found in the folders.
## Scope of work
Scope of retrospective analysis of management.
![](https://github.com/RickWeng/pca-collection/blob/master/scope/figures/scopeofreview.png)
## Cape Breton National Park
Assessing the most effective method of management.
![](https://github.com/RickWeng/pca-collection/blob/master/cbhnp/figures/browse-CI.png)
![](https://github.com/RickWeng/pca-collection/blob/master/cbhnp/figures/histogram-browse.png)
![](https://github.com/RickWeng/pca-collection/blob/master/cbhnp/figures/growth-WS.png)
![](https://github.com/RickWeng/pca-collection/blob/master/cbhnp/figures/Box-difference-WS.png)
![](https://github.com/RickWeng/pca-collection/blob/master/cbhnp/figures/difference-WS.png)
![](https://github.com/RickWeng/pca-collection/blob/master/cbhnp/figures/survival-species.png)
## Elk Island National Park
Data wrangling of all available historical data for further statistical analysis.   
Below is the sample code for cleaning 1999-2010 data. Other data cleaning processes can be found in the folder.
```
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
```
## Gros Morne National Park
Assessing the effectiveness of management and target achievement of CoRe projects.
![](https://github.com/RickWeng/pca-collection/blob/master/gmnp/figures/gmnp-moosepop.png)
![](https://github.com/RickWeng/pca-collection/blob/master/gmnp/figures/gmnp-woody-browse.png)
![](https://github.com/RickWeng/pca-collection/blob/master/gmnp/figures/gmnp-woody-growth.png)
## Point Pelee National Park
Assessing the effectiveness of management.
![](https://github.com/RickWeng/pca-collection/blob/master/ppnp/figures/Percent5Q1.png)
![](https://github.com/RickWeng/pca-collection/blob/master/ppnp/figures/Question4.png)
![](https://github.com/RickWeng/pca-collection/blob/master/ppnp/figures/Question42.png)
![](https://github.com/RickWeng/pca-collection/blob/master/ppnp/figures/Sleepy%20Hollow_species.png)
![](https://github.com/RickWeng/pca-collection/blob/master/ppnp/figures/Q52.png)
![](https://github.com/RickWeng/pca-collection/blob/master/ppnp/figures/Q7_overall.png)
![](https://github.com/RickWeng/pca-collection/blob/master/ppnp/figures/Q7_individual.png)
## Terra Nova
Assessing target achievement of CoRe projects.    
Below is the sample preprocessing code.
```
# task: calculate target achievement of core projects tnnp
# author: ricky weng
# library
library(dplyr)
library(stringr)

# working directory
setwd("U:/TNNP data")

# preprocessing
bf <- read.csv("bf.moose.csv", stringsAsFactors = FALSE)
hw <- read.csv("hardwood.csv")
## edit typo
bf$site <- str_replace_all(bf$site, "bF", "bf")
## make location naming consistent
bf$site <- if_else(bf$site == "", tolower(bf$location), bf$site)
hw$site.code <- tolower(hw$site.code)

# analysis
## add impacted level of site
impact_site <- bf%>%
  filter(year == 2014, height > 10 & height < 250, site.type != "black spruce")%>%
  group_by(site)%>%
  summarize(num_stems = n())%>%
  mutate(impact = case_when(
    num_stems <= 160 ~ "s",
    num_stems > 160 ~ "l"))
## calculation
### sapling number
cleaned_sapling <- bf%>%
  filter(site.type != "black spruce", browsed != "na",height > 30 & height < 250)%>%
  inner_join(impact_site, by = "site")%>%
  group_by(impact, year)%>%
  summarize(count = n())
### seedling number
cleaned_seedling <- bf%>%
  filter(site.type != "black spruce", browsed != "na",height > 10 & height < 250)%>%
  inner_join(impact_site, by = "site")%>%
  group_by(impact, year)%>%
  summarize(count = n())
### hardwood browse rate
cleaned_hw <- hw%>%
  filter(site.type != "black spruce", browsed != "na")%>%
  inner_join(impact_site, by = c("site.code" = "site"))%>%
  group_by(impact, year, browsed)%>%
  summarize(count = n())%>%
  ungroup()%>%
  group_by(impact, year)%>%
  summarize(percent = sum(count[browsed == "b"])/sum(count))
```
## Thousand Islands National Park
Assessing the effectiveness of management.
![](https://github.com/RickWeng/pca-collection/blob/master/tinp/figures/tinp-spring-sum.png)
![](https://github.com/RickWeng/pca-collection/blob/master/tinp/figures/tinp-spring-prop.png)
![](https://github.com/RickWeng/pca-collection/blob/master/tinp/figures/tinp-sw-seedling.png)
![](https://github.com/RickWeng/pca-collection/blob/master/tinp/figures/tinp-dba-sp.png)
![](https://github.com/RickWeng/pca-collection/blob/master/tinp/figures/tinp-dbb-sp.png)
