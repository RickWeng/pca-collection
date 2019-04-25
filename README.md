# Collection of data wrangling, analysis, and visualization work at PCA
For security reasons, raw data, statistical analysis results, and reports are not provided here.
Only selected figures are presented.
## Scope of work

## Cape Breton National Park
![](https://github.com/RickWeng/pca-collection/blob/master/cbhnp/figures/browse-CI.png)
![](https://github.com/RickWeng/pca-collection/blob/master/cbhnp/figures/histogram-browse.png)
![](https://github.com/RickWeng/pca-collection/blob/master/cbhnp/figures/growth-WS.png)
![](https://github.com/RickWeng/pca-collection/blob/master/cbhnp/figures/Box-difference-WS.png)
![](https://github.com/RickWeng/pca-collection/blob/master/cbhnp/figures/difference-WS.png)
![](https://github.com/RickWeng/pca-collection/blob/master/cbhnp/figures/survival-species.png)
## Elk Island National Park

## Gros Morne National Park
![](https://github.com/RickWeng/pca-collection/blob/master/gmnp/figures/gmnp-moosepop.png)
![](https://github.com/RickWeng/pca-collection/blob/master/gmnp/figures/gmnp-woody-browse.png)
![](https://github.com/RickWeng/pca-collection/blob/master/gmnp/figures/gmnp-woody-growth.png)
## Point Pelee National Park

## Terra Nova
```
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
![](https://github.com/RickWeng/pca-collection/blob/master/tinp/figures/tinp-spring-sum.png)
![](https://github.com/RickWeng/pca-collection/blob/master/tinp/figures/tinp-spring-prop.png)
![](https://github.com/RickWeng/pca-collection/blob/master/tinp/figures/tinp-sw-seedling.png)
![](https://github.com/RickWeng/pca-collection/blob/master/tinp/figures/tinp-dba-sp.png)
![](https://github.com/RickWeng/pca-collection/blob/master/tinp/figures/tinp-dbb-sp.png)
