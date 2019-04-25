# task: 1994-1999 data cleaning einp
# author: ricky weng

# library
library(tidyverse) # dplyr
library(xlsx) # export as a workbook
# set working directory and read csv data file
setwd("U:/EINP data/1994&1996") 

# delete 1st and 3rd row of all 1994 files
file_list <- list.files(pattern = "94.csv")
file_list
for (i in seq_along(file_list)) {
  filename = file_list[[i]]
  # Read data in
  df <- read.table(filename, sep = ",", header = FALSE, fill = TRUE, stringsAsFactors = FALSE, na.strings = c(" ", "", "  "))
  df <- df[-c(1, 3), ]
  # change column name 
  df[1, c(17:24)] <- c("Avg Cover", "% Comp", "Prom Value", "% Prom Value", "Native Graz.", "Native	% Clx", "Natural	Graz.", "Natural % Clx")
  df <- write.table(df, gsub(filename, pattern="94.csv", replacement = "94-1-3.csv"), na = "", sep = ",", row.names = FALSE, col.names = FALSE)
}

# get species cover data cleaned and combine 94 data to one sheet
file_list <- list.files(pattern = "94-1-3.csv")
file_list
# get all data frames of 1994 data
All <- lapply(file_list, function(i){
  df <- read.table(i, sep = ",", header = TRUE, check.names = FALSE, na.strings = c(" ", "", "  "))
  df <- df[ ,-c(17:24)]
  df <- df[-c(47:61), ]
  df <- df%>%
    gather(Plot, Cover, 2:16, na.rm = TRUE)%>%
    mutate(Site = gsub(i, pattern = "94-1-3.csv", replacement = ""), Year = 1994)
  assign(i, df)
})
# combine to one 
df <- do.call(rbind.data.frame, All)
write.table(df, "1994.csv", sep = ",", row.names = FALSE)

# delete 1st row of all 1996 files
# list 1996 files
file_list = grep(list.files(),pattern="94", inv=T, value=T)
file_list
for (i in seq_along(file_list)) {
  filename = file_list[[i]]
  # read data in
  df <- read.table(filename,sep=",",header=FALSE,fill=TRUE,stringsAsFactors = FALSE, na.strings = c(" ","","  "))
  df <- df[-1,]
  # change column name 
  df[1,c(17:24)]=c("Avg Cover","% Comp","Prom Value","% Prom Value","Native Graz.","Native	% Clx","Natural	Graz.","Natural % Clx")
  df <- write.table(df,gsub(filename,pattern=".csv",replacement="96-1.csv"),na="",sep=",",row.names = FALSE,col.names = FALSE)
}

# edits
# the first column name of original mosslk 1996 file is empty, change it to "Species"
MOSSLK <- read.table("MOSSLK96-1.csv", sep = ",", header = TRUE, check.names = FALSE, na.strings = c(" ", "", "  "))
names(MOSSLK)[1] <- "Species"
write.table(MOSSLK, "MOSSLK96-1.csv", na = "", sep = ",", row.names = FALSE)
# the 7th column name of original ISOWEST 1996 file is empty, change it to 6
ISOWEST <- read.table("ISOWEST96-1.csv", sep = ",", header = TRUE, check.names = FALSE, na.strings = c(" ", "", "  "))
names(ISOWEST)[7] <- 6
write.table(MOSSLK, "ISOWEST96-1.csv", na = "", sep = ",", row.names = FALSE)
HAYBURG <- read.table("HAYBURG96-1.csv", sep = ",", header = TRUE, check.names = FALSE, na.strings = c(" ", "", "  "))
names(HAYBURG)[8] <- 7
write.table(HAYBURG, "HAYBURG96-1.csv", na = "", sep = ",", row.names = FALSE)

# get species cover data cleaned and combine 96 data to one sheet
file_list <- list.files(pattern = "96-1.csv")
file_list
# get all data frames of 1996 data
All <- lapply(file_list, function(i){
  df <- read.table(i, sep = ",", header = TRUE, check.names = FALSE, na.strings = c(" ", "", "  "))
  df <- df[ ,-c(17:24)]
  df <- df%>%
    gather(Plot,Cover, 2:16, na.rm = TRUE)%>%
    mutate(Site = gsub(i, pattern = "96-1.csv", replacement = ""), Year = 1996)
  assign(i, df)
})

# combine to one 
df <- do.call(rbind.data.frame, All)
# filter to exclude non species
df <- df%>%
  filter(!(Species %in% c("BARE SOIL", "LITTER", "MOSS", "NT  RATING", "NT RGE CND", "NZ  RATING", "NZ RGE CND", "SOIL TEXT.", "TOTAL VEG.", "bare soil", "litter", "moss", "total veg", "TOTAL VEG", "Bare Grd.", "Litter", "Moss", "Total Veg. ", "BARESOIL", "TOTALVEG.")))
write.table(df, "1996.csv", sep = ",", row.names = FALSE)

# combine 1994 and 1996 data
year94 <- read.table("1994.csv", sep = ",", header = TRUE, check.names = FALSE, na.strings = c(" ", "", "  "))
year96 <- read.table("1996.csv", sep = ",", header = TRUE, check.names = FALSE, na.strings = c(" ", "", "  "))
year9496 <- bind_rows(year94, year96)
# change species consistently to lowercase
year9496$Species <- tolower(year9496$Species)
# change "na" to blank
year9496$Species <- gsub("na", "", fixed = TRUE, year9496$Species)
write.table(year9496, "1994&1996.csv", sep = ",", row.names = FALSE, na = "")

# combine calculated species data 
# 1994
file_list <- list.files(pattern = "94-1-3.csv")
file_list
# get all data frames of 1994 data
All <- lapply(file_list, function(i) {
  df <- read.table(i, sep = ",", header = TRUE, check.names = FALSE, na.strings = c(" ", "", "  "))
  df <- df[ ,c(1,17:24)]
  df <- df[-c(47:61), ]
  df <- df%>%
    mutate(Site = gsub(i, pattern = "94-1-3.csv", replacement = ""), Year = 1994)%>%
    filter(!is.na(Species))
  assign(i, df)
})
# combine to one 
calcualted94 <- do.call(rbind.data.frame, All)

# 1996
file_list <- list.files(pattern = "96-1.csv")
file_list
# get all data frames of 1996 data
All <- lapply(file_list, function(i) {
  df <- read.table(i, sep = ",", header = TRUE, check.names = FALSE, na.strings = c(" ", "", "  "))
  df <- df[ ,c(1, 17:24)]
  df <- df%>%
    mutate(Site = gsub(i, pattern = "96-1.csv", replacement = ""), Year = 1996)%>%
    filter(Site == "TAWEXCL"|!is.na(Species))%>%
    filter(!(Species %in% c("BARE SOIL", "LITTER", "MOSS", "NT  RATING", "NT RGE CND", "NZ  RATING", "NZ RGE CND", "SOIL TEXT.", "TOTAL VEG.", "bare soil", "litter", "moss", "total veg", "TOTAL VEG", "Bare Grd.", "Litter", "Moss", "Total Veg. ", "BARESOIL", "TOTALVEG.")))
  assign(i, df)
})
# combine to one 
calcualted96 <- do.call(rbind.data.frame, All)
calcualted96 <- calcualted96[-c(385:419), ]
# combine 1994 and 1996 calculated data
calcualted9496 <- bind_rows(calcualted94, calcualted96)
# change species consistently to lowercase
calcualted9496$Species <- tolower(calcualted9496$Species)
write.table(calcualted9496, "1994&1996 Calculated Results.csv", sep = ",", row.names = FALSE, na = "")

# combine plot general information
# 1994
file_list <- list.files(pattern = "94-1-3.csv")
file_list
# get all data frames of 1994 data
All <- lapply(file_list, function(i){
  df <- read.table(i, sep = ",", header = TRUE, check.names = FALSE, na.strings = c(" ", "", "  "))
  df <- df[ ,c(1:16)]
  df <- df[c(47:61), ]
  df <- df%>%
    mutate(Site = gsub(i, pattern = "94-1-3.csv", replacement = ""), Year = 1994)%>%
    filter(!is.na(Species))
  assign(i, df)
})
# combine to one 
general94 <- do.call(rbind.data.frame, All)
# remove white space and change to consistent form
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
general94$Species <- trim(general94$Species)
general94$Species <- gsub(general94$Species, pattern = "MOSS", replacement = "Moss", fixed = TRUE)
general94$Species <- gsub(general94$Species, pattern = "LITTER", replacement = "Litter", fixed = TRUE)
general94$Species <- gsub(general94$Species, pattern = "TOTAL VEG", replacement = "Total Veg", fixed = TRUE)
general94$Species <- gsub(general94$Species, pattern = "Soil Text.", replacement = "Soil Text", fixed = TRUE)
general94$Species <- gsub(general94$Species, pattern = "BARE SOIL", replacement = "Bare Soil", fixed = TRUE)
general94$Species <- gsub(general94$Species, pattern = "Nz Rg cond", replacement = "Nz Rg Cond", fixed = TRUE)
general94 <- general94%>%
  gather(key = "Plot", value = "Cover", 2:16)
general94 <- general94%>%
  spread(Species, Cover)

# 1996
file_list <- list.files(pattern = "96-1.csv")
file_list
# get all data frames of 1996 data
All <- lapply(file_list, function (i){
  df <- read.table(i, sep = ",", header = TRUE, check.names = FALSE, na.strings = c(" ", "", "  "), strip.white = TRUE)
  df <- df[ , c(1:16)]
  df <- df%>%
    mutate(Site = gsub(i, pattern = "96-1.csv", replacement = ""), Year = 1996)%>%
    filter(Site == "TAWEXCL"|!is.na(Species))%>%
    filter(Species %in% c("BARE SOIL", "LITTER", "MOSS", "NT  RATING", "NT RGE CND", "NZ  RATING", "NZ RGE CND", "SOIL TEXT.", "TOTAL VEG.", "bare soil", "litter", "moss", "total veg", "TOTAL VEG", "Bare Grd.", "Litter", "Moss", "Total Veg. ", "BARESOIL", "TOTALVEG."))
  assign(i, df)
})
# combine to one 
general96 <- do.call(rbind.data.frame, All)
general96 <- general96[-c(22:25), ]

# remove white space and change to consistent form
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
general96$Species <- trim(general96$Species)

general96$Species <- gsub(general96$Species, pattern = "MOSS|moss", replacement = "Moss")
general96$Species <- gsub(general96$Species, pattern = "BARE SOIL|bare soil|Bare Grd.|BARESOIL", replacement = "Bare Soil")
general96$Species <- gsub(general96$Species, pattern = "LITTER|litter", replacement = "Litter")
general96$Species <- gsub(general96$Species, pattern = "SOIL TEXT.", replacement = "Soil Text", fixed = TRUE)
general96$Species <- gsub(general96$Species, pattern = "Total Veg.|total veg|TOTAL VEG|TOTALVEG.|TOTAL VEG.", replacement = "Total Veg")
general96$Species <- gsub(general96$Species, pattern = "NZ RGE CND", replacement="Nz Rg Cond", fixed = TRUE)
general96$Species <- gsub(general96$Species, pattern = "NT RGE CND", replacement="Ntv Rg Cond", fixed = TRUE)
general96$Species <- gsub(general96$Species, pattern = "NT  RATING", replacement="Ntv Actual", fixed = TRUE)
general96$Species <- gsub(general96$Species, pattern = "NZ  RATING", replacement="Nz Actual", fixed = TRUE)

general96 <- general96%>%
  gather(key = "Plot", value = "Cover", 2:16)
general96 <- general96%>%
  spread(Species, Cover)
# combine 1994 and 1996 calculated data
general9496 <- bind_rows(general94, general96)
library(plyr)
general9496 <- rbind.fill(general94, general96)
detach(package:plyr)

write.table(general9496, "1994&1996 Plot Information.csv", sep = ",", row.names = FALSE, na = "")

# combine "1994&1996", "1994&1996 Calculated Results", "1994&1996 Plot Information" to one workbook
data9496 <- read.table("1994&1996.csv", sep = ",", header = TRUE, check.names = FALSE, na.strings = "")
calculated9496 <- read.table("1994&1996 Calculated Results.csv", sep = ",", header = TRUE, check.names = FALSE, na.strings = "")
information9496 <- read.table("1994&1996 Plot Information.csv", sep = ",", header = TRUE, check.names = FALSE, na.strings = "")
write.xlsx(data9496, file = "Rangeland Reference 1994-2010 data Osko_2019-01-24.xlsx", 
           sheetName = "1994&1996", append = TRUE)
write.xlsx(calculated9496, file = "Rangeland Reference 1994-2010 data Osko_2019-01-24.xlsx", 
           sheetName = "1994&1996 Calculated Results", append = TRUE)
write.xlsx(information9496, file = "Rangeland Reference 1994-2010 data Osko_2019-01-24.xlsx", 
           sheetName = "1994&1996 Plot Information", append = TRUE)



  


