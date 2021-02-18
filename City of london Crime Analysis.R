library(tidyverse)
library(plyr)
library(dplyr)
library(zoo)
library(lubridate)
library(directlabels)
library(reactable)
library(ztable)
library(magrittr)
library(data.table)
library(waffle)
library(Amelia)
library(GGally)
library(ggcorrplot)

library(ggpubr)
library(GGally)

library(caret)
library(lattice)
library(caretEnsemble)
library(psych)
library(Amelia)
library(mice)

library(rpart)
library(randomForest)

library(tmap)
library(rgdal)
library(dplyr)


#########################
# Getting the data
# Create a path 
path = "C:/Users/ASUS/Downloads/final assignments/data science/full data set/New folder (2)"
path2 = "C:/Users/ASUS/Downloads/final assignments/data science/full data set/Outcome"
path3 = "C:/Users/ASUS/Downloads/final assignments/data science/full data set/StopandSearch"

# combining all the files into one file from the path

dir(path = path, pattern='csv$', recursive = TRUE, full.names = TRUE) %>%
  lapply(FUN = read.csv) %>%
  bind_rows() %>%
  write.csv("london_crime.csv")

dir(path2 = path, pattern='csv$', recursive = TRUE, full.names = TRUE) %>%
  lapply(FUN = read.csv) %>%
  bind_rows() %>%
  write.csv("london_ outcome_combined_csv.csv")

dir(path3 = path, pattern='csv$', recursive = TRUE, full.names = TRUE) %>%
  lapply(FUN = read.csv) %>%
  bind_rows() %>%
  write.csv("london_stop_search.csv")

##################################################################

# Reading the data 
outcome <- read.csv("C:/Users/ASUS/Downloads/final assignments/data science/full data set/Final Data/london_ outcome_combined_csv.csv",header = TRUE)
stop_and_search <- read.csv("C:/Users/ASUS/Downloads/final assignments/data science/full data set/Final Data/london_stop_search.csv",header = TRUE)
crime <- read.csv("C:/Users/ASUS/Downloads/final assignments/data science/full data set/Final Data/london_crime.csv",header = TRUE)



# Convert the Month Variable to Date
crime <- mutate(crime,new_date = as.Date(as.yearmon(crime$Month)))
head(crime)
outcome <- mutate(outcome,new_date = as.Date(as.yearmon(outcome$Month)))

# Dividing the month column into two different columns
crime$year <- year(ymd(crime$new_date))
crime$month <- month(ymd(crime$new_date))
summary(crime)
str(crime)

outcome$year <- year(ymd(outcome$new_date))
outcome$month <- month(ymd(outcome$new_date))

# Another way to divide date
# crime <- crime %>%
# separate(new_date, into = c("Year", "Month", "date"), sep = "-")
crime = subset(crime, select = -c(Context,Reported.by,new_date) )

outcome = subset(outcome, select = -c(ï..Crime.ID,Location,Reported.by,Falls.within,new_date) )

#crime$Month <- month.abb[as.numeric(crime$Month)]

# Setting blanks as NA

head(crime)
crime[crime == ""] <- NA
outcome[outcome == ""] <- NA
stop_and_search[stop_and_search == ""] <- NA

# For Data Cleaning 
# NA and Empty rows are not removed in case of EDA since these are 
# recorded data and removing the entries will mean distortion of 
# original data. Data cleaning is done at many places but for initial 
# EDA i am using the original Dataset to get the original data view
# 
#---------------------------------------------------------------

#crime<- na.omit(crime) 

#outcome<- na.omit(outcome) 

#stop_and_search<- na.omit(stop_and_search) 

#---------------------------------------------------------------

ttl <- crime %>% group_by(year,Crime.type) %>% dplyr::summarise(count=n())
year_crime<- crime %>% group_by(year) %>% dplyr::summarise(count=n()) %>% mutate(pct_change = ((count/lag(count) - 1) * 100))


#-------------- Annual Percentage Change in Crime ---------

# newData <- subset(newData, select = -c(year))
year_crime$pct_change[2]<- NA
year_crime$pct_change<- round(year_crime$pct_change,2) 

yearly_percent_crimeCount_bar <- count_geom_bar(year_crime,year_crime$year,year_crime$count,"YEAR","Count","Fig 1 - Crime Count Per Year","Crime Data Analysis during 2017-20 - City of London")
yearly_percent_crimeCount_bar

yearly_percent_crimeChange_bar <- count_geom_bar(year_crime,year_crime$year,year_crime$pct_change,"YEAR","Percent","Fig 2 - Annual Percent Change in Crime","Crime Data Analysis during 2017-20 - City of London")
yearly_percent_crimeChange_bar


#-----------------------------------------------------------------------

# Divide the data into into four different datasets to analyse Each Year 

#################################################

crime2017 = subset(london_full, year=='2017')
crime2018 = subset(london_full, year=='2018')
crime2019 = subset(london_full, year=='2019')
crime2020 = subset(london_full, year=='2020')

# Year 2017
londonShape<-readOGR(dsn="C:/Users/ASUS/Downloads/final assignments/data science/full data set/Final Data/boundry/supbound", layer="england_lsoa_2011")

numCrimesByLSOA2017<-crime2017 %>%
  select(LSOA.code, LSOA.name, Crime.type) %>%
  group_by(LSOA.code) %>%
  dplyr::summarise(Num.crimes=n())

londonShape@data<-left_join(londonShape@data, numCrimesByLSOA2017,
                            by=c('code'='LSOA.code'))
tmap_mode("view")
tm_shape(londonShape) +
  tm_fill("Num.crimes", alpha=0.5, style="kmeans", border.col = "black") +
  tm_borders(alpha=0.5)+
  tm_layout(main.title = "Year 2017")

# Year 2018

londonShape2<-readOGR(dsn="C:/Users/ASUS/Downloads/final assignments/data science/full data set/Final Data/boundry/supbound", layer="england_lsoa_2011")
numCrimesByLSOA2018<-crime2018 %>%
  select(LSOA.code, LSOA.name, Crime.type) %>%
  group_by(LSOA.code) %>%
  dplyr::summarise(Num.crimes=n())

londonShape2@data<-left_join(londonShape2@data, numCrimesByLSOA2018,
                             by=c('code'='LSOA.code'))

tm_shape(londonShape2) +
  tm_fill("Num.crimes", alpha=0.5, style="kmeans", border.col = "black") +
  tm_borders(alpha=0.5)+
  tm_layout(title = "Year 2018")



# Map for year 2019

londonShape3<-readOGR(dsn="C:/Users/ASUS/Downloads/final assignments/data science/full data set/Final Data/boundry/supbound", layer="england_lsoa_2011")
numCrimesByLSOA2019<-crime2019 %>%
  select(LSOA.code, LSOA.name, Crime.type) %>%
  group_by(LSOA.code) %>%
  dplyr::summarise(Num.crimes=n())

londonShape3@data<-left_join(londonShape3@data, numCrimesByLSOA2019,
                             by=c('code'='LSOA.code'))

tm_shape(londonShape3) +
  tm_fill("Num.crimes", alpha=0.5, style="kmeans", border.col = "black") +
  tm_borders(alpha=0.5)+
  tm_layout(title = "Year 2019")



# MAp for year 2020
londonShape4<-readOGR(dsn="C:/Users/ASUS/Downloads/final assignments/data science/full data set/Final Data/boundry/supbound", layer="england_lsoa_2011")
numCrimesByLSOA2020<-crime2020 %>%
  select(LSOA.code, LSOA.name, Crime.type) %>%
  group_by(LSOA.code) %>%
  dplyr::summarise(Num.crimes=n())

londonShape4@data<-left_join(londonShape4@data, numCrimesByLSOA2020,
                             by=c('code'='LSOA.code'))

tm_shape(londonShape4) +
  tm_fill("Num.crimes", alpha=0.5, style="kmeans", border.col = "black") +
  tm_borders(alpha=0.5)+
  tm_layout(title = "Year 2020")


# -----------------------------------------------------------


# Heat Maps and Pivot Tables using Ztable 

y=table(crime$year,crime$month)

options(ztable.type="html")
z=ztable(y) 
z %>% makeHeatmap() %>% print(type = "viewer",caption="HeatMap 1.  Heatmap Table")

z %>% 
  makeHeatmap(palette="Blues") %>%
  print(type = "viewer",caption="HeatMap 1. Crime Count - Every Month Per Year ")




#crime2017$month <- month.abb[as.numeric(crime2017$month)]

heatmap_2017=table(crime2017$Crime.type,crime2017$month)
options(ztable.type="html")
heatmap_2017z=ztable(heatmap_2017) 
heatmap_2017z %>% makeHeatmap() %>% print(type = "viewer",caption="Heat Map 2 Freq. Dist. of each Crime Type (year 2017)")


#crime2018$month <- month.abb[as.numeric(crime2018$month)]

heatmap_2018=table(crime2018$Crime.type,crime2018$month)
options(ztable.type="html")
heatmap_2018z=ztable(heatmap_2018) 
heatmap_2018z %>% makeHeatmap() %>% print(type = "viewer",caption="Heat Map 3 Freq. Dist. of each Crime Type (year 2018)")

heatmap_2019=table(crime2019$Crime.type,crime2019$month)
options(ztable.type="html")
heatmap_2019z=ztable(heatmap_2019) 
heatmap_2019z %>% makeHeatmap() %>% print(type = "viewer",caption="Heat Map 4 Freq. Dist. of each Crime Type (year 2019)")

x=table(crime2020$Crime.type,crime2020$month)
options(ztable.type="html")
z=ztable(x) 
z %>% makeHeatmap() %>% print(type = "viewer",caption="Heat Map 5. Freq. Dist. of each Crime Type (year 2020)")

x=table(crime$Crime.type,crime$year)
options(ztable.type="html")
z=ztable(x) 
z %>% makeHeatmap() %>% print(type = "viewer",caption="Heat Map 6 Freq. Dist. of each Crime Type per year (2017-2020)")

#-------------------------------------------------------------------

# Heat Map Type 2

ttl_count_crime<- crime %>% group_by(Crime.type,year) %>% dplyr::summarise(count=n())

ttl_count_crime<- ttl_count_crime %>%
  pivot_wider(names_from = year, values_from = c(count))

sum2017 <- sum(ttl_count_crime$`2017`)
sum2018 <- sum(ttl_count_crime$`2018`)
sum2019 <- sum(ttl_count_crime$`2019`)
sum2020 <- sum(ttl_count_crime$`2020`)
sum2017

ttl_count_crime<-ttl_count_crime %>%
  mutate(Perc2017 = (`2017`/sum2017)*100)%>% 
  mutate(perc2018 = (`2018`/sum2018)*100)%>% 
  mutate(perc2019 = (`2019`/sum2019)*100)%>% 
  mutate(perc2020 = (`2020`/sum2020)*100)

ttl_count_crime_yrChange<-ttl_count_crime %>%
  mutate(PChange2018 = ((`2018`-`2017`)/`2017`))%>% 
  mutate(PChange2019 = ((`2019`-`2018`)/`2018`))%>% 
  mutate(PChange2020 = ((`2020`-`2019`)/`2019`)) 
ttl_count_crime_yrChange = subset(ttl_count_crime_yrChange, select = -c(PChange2018,`2017`,`2018`,`2019`,`2020`,Perc2017,perc2018,perc2019,perc2020) )

ttl_count_crime_yrChange <- melt(setDT(ttl_count_crime_yrChange), id.vars = "Crime.type", variable.name = "year")
names(ttl_count_crime_yrChange)[names(ttl_count_crime_yrChange) == "value"] <- "Percentage"

ttl_count_crime_yrChange$Percentage<- round(ttl_count_crime_yrChange$Percentage,2) 

levels(ttl_count_crime_yrChange$year) <- c(levels(ttl_count_crime_yrChange$year), "2019","2020")

ttl_count_crime_yrChange$year[ttl_count_crime_yrChange$year ==  "PChange2019"] <- "2019"
ttl_count_crime_yrChange$year[ttl_count_crime_yrChange$year ==  "PChange2020"] <- "2020"

ttl_percent_crimeChange_bar <- count_geom_bar_dodge_percent_3(ttl_count_crime_yrChange,ttl_count_crime_yrChange$Crime.type,ttl_count_crime_yrChange$year,ttl_count_crime_yrChange$Percentage,"YEAR","Percent","Fig 2-1 - Annual Percent Change in Crime","Crime Data Analysis during 2017-20 - City of London")
ttl_percent_crimeChange_bar



chk_sum<- sum(ttl_count_crime$perc2018)  
chk_sum
crimePerc = subset(ttl_count_crime, select = -c(`2017`,`2018`,`2019`,`2020`) )
crimePerc
names(crimePerc)[names(crimePerc) == "Perc2017"] <- 2017
names(crimePerc)[names(crimePerc) == "perc2018"] <- 2018
names(crimePerc)[names(crimePerc) == "perc2019"] <- 2019
names(crimePerc)[names(crimePerc) == "perc2020"] <- 2020

newData <- data.frame(crimePerc, row.names = crimePerc$Crime.type)
newData

myDF<-newData[,2:5]
crimeheatMatr <- data.matrix(myDF)

# Percentage Distribution of Each Crime Type during 2017-20
heatmap(crimeheatMatr, Rowv=NA, Colv=NA, col=heat.colors(256), scale="column",
        margins=c(5,10))

#----------------------------------------------------------------

# Crime Type Frequency and Percentage Distribution
crimetypeTotal$pct<- round(crimetypeTotal$pct,2) 
crime_count_bar_total_new <- count_geom_bar2(crimetypeTotal,crimetypeTotal$count,crimetypeTotal$Crime.type,"Count","Types of Crime","Fig 3 - Freq. Distribution of Different Crime types","Crime Data Analysis during 2017-20 - City of London")
crime_count_bar_total_new

crime_count_bar_new_percent <- count_geom_bar_percent_vert(crimetypeTotal,crimetypeTotal$pct,crimetypeTotal$Crime.type,"Percentage","Types of Crime","Fig 4 - Percentage Distribution of Different Crime types","Crime Data Analysis during 2017-20 - City of London")
crime_count_bar_new_percent


# Count of Crime type in Each year

crimetypeTotal <- crime %>% group_by(Crime.type) %>% dplyr::summarise(count=n())%>% mutate(pct = count/sum(count))
crimetypeTotal2017 <- crime %>% filter(year=='2017') %>% group_by(Crime.type) %>% dplyr::summarise(count=n())
crimetypeTotal2018 <- crime %>% filter(year=='2018') %>% group_by(Crime.type) %>% dplyr::summarise(count=n())
crimetypeTotal2019 <- crime %>% filter(year=='2019') %>% group_by(Crime.type) %>% dplyr::summarise(count=n())
crimetypeTotal2020 <- crime %>% filter(year=='2020') %>% group_by(Crime.type) %>% dplyr::summarise(count=n())


crime_count_bar_overall <- count_geom_bar_withTheme(crimetypeTotal,crimetypeTotal$Crime.type,crimetypeTotal$count,"Type of crime","Count","Fig 5 -Frequency Dist. - Type of Crime","Crime Data Analysis during 2017-20 - City of London")

crime_count_bar2017 <- count_geom_bar_withTheme(crimetypeTotal2017,crimetypeTotal2017$Crime.type,crimetypeTotal2017$count,"Type of crime","Count","Fig 6 -Frequency Dist. - Type of Crime - 2017","Crime Data Analysis during 2017-20 - City of London")
crime_count_bar2018 <- count_geom_bar_withTheme(crimetypeTotal2018,crimetypeTotal2018$Crime.type,crimetypeTotal2018$count,"Type of crime","Count","Fig 7 -Frequency Dist. - Type of Crime - 2018","Crime Data Analysis during 2017-20 - City of London")
crime_count_bar2019 <- count_geom_bar_withTheme(crimetypeTotal2019,crimetypeTotal2019$Crime.type,crimetypeTotal2019$count,"Type of crime","Count","Fig 8 -Frequency Dist. - Type of Crime - 2019","Crime Data Analysis during 2017-20 - City of London")
crime_count_bar2020 <- count_geom_bar_withTheme(crimetypeTotal2020,crimetypeTotal2020$Crime.type,crimetypeTotal2020$count,"Type of crime","Count","Fig 9 -Frequency Dist. - Type of Crime - 2020","Crime Data Analysis during 2017-20 - City of London")

crime_count_bar_overall
crime_count_bar2017
crime_count_bar2018
crime_count_bar2019
crime_count_bar2020



#############-------------------------#################


# Crime count percentage as percentage of total crime in that period
#CTTP <- Crime Type Total Percentage
CTTP <- crime %>% group_by(Crime.type) %>% dplyr::summarise(count=n()) %>% mutate(pct = count/sum(count))
CTTP2017 <- crime %>% filter(year=='2017') %>% group_by(Crime.type) %>% dplyr::summarise(count=n())%>% mutate(pct = count/sum(count))
CTTP2018 <- crime %>% filter(year=='2018') %>% group_by(Crime.type) %>% dplyr::summarise(count=n())%>% mutate(pct = count/sum(count))
CTTP2019 <- crime %>% filter(year=='2019') %>% group_by(Crime.type) %>% dplyr::summarise(count=n())%>% mutate(pct = count/sum(count))
CTTP2020 <- crime %>% filter(year=='2020') %>% group_by(Crime.type) %>% dplyr::summarise(count=n())%>% mutate(pct = count/sum(count))

CTTP_overall <- count_geom_bar_percent(CTTP,CTTP$Crime.type,CTTP$pct,"Type of crime","Percent","Fig 10 - Percentage Dist. Type of Crime","Crime Data Analysis during 2017-20 - City of London")


CTTP2 <- crime %>% group_by(Crime.type) %>% dplyr::summarise(count=n()) %>% mutate(pct = (count/sum(count)*100))

ggplot(CTTP2, aes(fill=Crime.type, values=pct)) + geom_waffle(n_rows=4, colour="white")+
  theme(axis.line = element_blank(), panel.background = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) + coord_equal() +
  labs(x=NULL, y=NULL, fill="Crime.type",
       title="Proportion of crime",
       caption="Crime Proportion")

library(treemapify)
ggplot(CTTP2, aes(area = count, fill = Crime.type, label=Crime.type)) +
  geom_treemap() + geom_treemap_text() +
  labs(title="Crime during 2017-2020",
       fill="Count of Crime",
       caption="Distribution of different Crime")


CTTP_bar2017 <- count_geom_bar_percent(CTTP2017,CTTP2017$Crime.type,CTTP2017$pct,"Type of crime","Percent","Fig 11- Perc. Dist. Type of Crime - 2017","Crime Data Analysis during 2017-20 - City of London")
CTTP_bar2018 <- count_geom_bar_percent(CTTP2018,CTTP2018$Crime.type,CTTP2018$pct,"Type of crime","Percent","Fig 12 - Perc. Dist. Type of Crime - 2018","Crime Data Analysis during 2017-20 - City of London")
CTTP_bar2019 <- count_geom_bar_percent(CTTP2019,CTTP2019$Crime.type,CTTP2019$pct,"Type of crime","Percent","Fig 13 - Perc. Dist. Type of Crime - 2019","Crime Data Analysis during 2017-20 - City of London")
CTTP_bar2020 <- count_geom_bar_percent(CTTP2020,CTTP2020$Crime.type,CTTP2020$pct,"Type of crime","Percent","Fig 14 - Perc. Dist. Type of Crime - 2020","Crime Data Analysis during 2017-20 - City of London")

CTTP_overall
CTTP_bar2017
CTTP_bar2018
CTTP_bar2019
CTTP_bar2020


# Crime tYpe - Year Dodge Chart - by count  
year_month_df<- crime %>% group_by(year,month) %>% dplyr::summarise(count=n()) 
year_month_count <- count_geom_bar_grid_count_2(year_month_df,year_month_df$year,year_month_df$month,year_month_df$count,"Year","Count","Fig 15 - Annual Freq Dist. of crime Per Month","Crime Data Analysis during 2017-20 - City of London")
year_month_count

# crime in each month as percentage of total crime in that year 
year_month_df_percentage<- crime %>% group_by(year,month) %>% dplyr::summarise(count=n())

year_month_df_percentage<-year_month_df_percentage[!(is.na(year_month_df_percentage$year) | year_month_df_percentage$year==""), ]
year_month_df_percentage<-year_month_df_percentage[!(is.na(year_month_df_percentage$month) | year_month_df_percentage$month==""), ]

year_month_df_percentage$year<- factor(year_month_df_percentage$year)


var.level <- levels(year_month_df_percentage$year)


year_month_df_percentage<- find_percentage_dataset(year_month_df_percentage,"year","month")
# These two lines wors but changes the order of graph
#year_month_df_percentage$month<-month.abb[as.numeric(year_month_df_percentage$month)]
year_month_df_percentage$month<-as.factor(year_month_df_percentage$month) 
year_month_df_percentage_bar <- count_geom_bar_grid_count_percentage(year_month_df_percentage,year_month_df_percentage$year,year_month_df_percentage$month,year_month_df_percentage$Percentage,"Year","Percent","Fig 16 - Crime Annual Percentage Distribution per Month ","Crime Data Analysis during 2017-20 - City of London")
year_month_df_percentage_bar


crime_type_df_perc <- crime %>% group_by(Crime.type,year) %>% dplyr::summarise(count=n()) 

crime_type_df_perc<-crime_type_df_perc[!(is.na(crime_type_df_perc$year) | crime_type_df_perc$year==""), ]
crime_type_df_perc<-crime_type_df_perc[!(is.na(crime_type_df_perc$Crime.type) | crime_type_df_perc$Crime.type==""), ]

crime_type_df_perc$year<- factor(crime_type_df_perc$year)
crime_type_df_perc<- find_percentage_dataset(crime_type_df_perc,"year","Crime.type")

crime_type_year_perc_bar <- count_geom_bar_grid_count_percentage(crime_type_df_perc,crime_type_df_perc$Crime.type,crime_type_df_perc$year,crime_type_df_perc$Percentage,"Year","Percent","Fig 17 - Annual Percentage Distribution - Different Crime type ","Crime Data Analysis during 2017-20 - City of London")
crime_type_year_perc_bar

crime_type_year_perc_dodge <- count_geom_bar_percent_dodge(crime_type_df_perc,crime_type_df_perc$Crime.type,crime_type_df_perc$year,crime_type_df_perc$Percentage,"Year","Percent","Fig 18 - Annual Percentage Distribution Different Crime type","Crime Data Analysis during 2017-20 - City of London")
crime_type_year_perc_dodge



# crime_type_df_perc<- add_percent_columns(crime_type_df_perc,"year","month")


#-------------------------------------------------------------------



crime_month_count<- crime %>% group_by(Crime.type,month) %>% dplyr::summarise(count=n())
crime_month_count$month<- as.factor(crime_month_count$month)
crime_month_count_line1 <- count_geom_line_fwrap(crime_month_count,crime_month_count$month,crime_month_count$count,crime_month_count$Crime.type,"Month","Count","Fig 19 - Annual Freq. Distribution Different Crime type","Crime Data Analysis during 2017-20 - City of London")
crime_month_count_line1


crime_month_year_ctype<- crime %>% group_by(Crime.type,year,month) %>% dplyr::summarise(count=n())
#crime_month_year_ctype$month<- as.factor(crime_month_year_ctype$month)
#crime_month_year_ctype$year<- as.factor(crime_month_year_ctype$year)
crime_month_count_line2 <- count_geom_col_fwrap(crime_month_year_ctype,crime_month_year_ctype$year,crime_month_year_ctype$month,crime_month_year_ctype$count,crime_month_year_ctype$Crime.type,"Month","Count","Fig 20 - Annual Freq Distribution Different Crime type","Crime Data Analysis during 2017-20 - City of London")
crime_month_count_line2


#crime frequesncy in different year on basis of month to check pattern 
crime_month_count_line3 <- count_geom_line_fwrap2(crime_month_year_ctype,crime_month_year_ctype$year,crime_month_year_ctype$month,crime_month_year_ctype$count,crime_month_year_ctype$Crime.type,"Month","Count","Fig 21 - Annual Percentage Distribution Different Crime type","Crime Data Analysis during 2017-20 - City of London")
crime_month_count_line3

crime_month_count_line4 <- count_geom_col_fwrap2(crime_month_year_ctype,crime_month_year_ctype$year,crime_month_year_ctype$month,crime_month_year_ctype$count,crime_month_year_ctype$Crime.type,"Month","Count","Fig 22 - Annual Percentage Distribution Different Crime type","Crime Data Analysis during 2017-20 - City of London")
crime_month_count_line4


#-----------------------------------------------------------------

# Stop And Search EDA
#-----------------------------------------------------------------

head(stop_and_search)
gender<- stop_and_search %>% group_by(Gender,Outcome) %>% dplyr::summarise(count=n())
gender_age<- stop_and_search %>% group_by(Gender,Age.range,Outcome) %>% dplyr::summarise(count=n())
age_range<- stop_and_search %>% group_by(Age.range,Outcome) %>% dplyr::summarise(count=n())
age_range_ethnicity<- stop_and_search %>% group_by(Age.range,Officer.defined.ethnicity,Outcome) %>% dplyr::summarise(count=n())
ethinicity<- stop_and_search %>% group_by(Officer.defined.ethnicity,Outcome) %>% dplyr::summarise(count=n())
gender_ethinicity<- stop_and_search %>% group_by(Gender,Officer.defined.ethnicity,Outcome) %>% dplyr::summarise(count=n())



# Gender based distribution
gender_count<- stop_and_search %>% group_by(Gender) %>% dplyr::summarise(count=n())%>% mutate(pct = count/sum(count))
gender_count

gender_count_percent_dist <- count_geom_bar_percent(gender_count,gender_count$Gender,gender_count$pct,"Gender","Percent","Fig 23 - Gender Freq Dist.- Stop and search ","Analysis of Stop and Search 2017-20")
gender_count_percent_dist



gender_count <-gender_count%>%
  mutate(lab.ypos = cumsum(pct) - 0.5*pct)
gender_count
ggplot(gender_count, aes(x="", y=pct, fill= factor(Gender)))+geom_bar(stat="identity")+
  coord_polar(theta="y")+
  theme(axis.line = element_blank(), panel.background = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  geom_text(aes(y=lab.ypos,label = scales::percent(pct,accuracy = .11) ),vjust =0,color ="black")+
  labs(x=NULL, y=NULL, fill="Gender",
       title="Fig 24 - Gender Proportions - Stop and Search London",
       caption="Stop and Search- City of London Analysis")


ethnicity_count<- stop_and_search %>% group_by(Officer.defined.ethnicity) %>% dplyr::summarise(count=n()) %>%mutate(pct = count/sum(count))

ethnicity_count_dist <- count_geom_bar(ethnicity_count,ethnicity_count$Officer.defined.ethnicity,ethnicity_count$count,"Officer Defined Ethnicity","Count","(Fig 25) Freq. Dist. - Officer Defined Ethnicity","Analysis of Stop and Search 2017-20")
ethnicity_count_dist

ethnicity_count_percent_dist <- count_geom_bar_percent(ethnicity_count,ethnicity_count$Officer.defined.ethnicity,ethnicity_count$pct,"Officer Defined Ethnicity","Percent"," Fig 26 - Percentage Dist. of Officer Defined Ethnicity","Analysis of Stop and Search 2017-20")
ethnicity_count_percent_dist

#Age based count
age_range_count<- stop_and_search %>% group_by(Age.range) %>% dplyr::summarise(count=n())%>%mutate(pct = count/sum(count))

age_range_count_dist <- count_geom_bar(age_range_count,age_range_count$Age.range,age_range_count$count,"Age Range","Count","Fig 27 - Freq. Dist. of Age Range","Analysis of Stop and Search 2017-20")
age_range_count_dist

age_range_count_percent_dist <- count_geom_bar_percent(age_range_count,age_range_count$Age.range,age_range_count$pct,"Age Range","Percent","Fig 28 Percentage Dist. of Age Range","Analysis of Stop and Search 2017-20")
age_range_count_percent_dist


#ethnicity based count
self_defined_ethnicity_count<- stop_and_search %>% group_by(Self.defined.ethnicity) %>% dplyr::summarise(count=n())%>%mutate(pct = count/sum(count))

self_defined_ethnicity_count_dist <- count_geom_bar(self_defined_ethnicity_count,self_defined_ethnicity_count$Self.defined.ethnicity,self_defined_ethnicity_count$count,"Self Defined Ethnicity","Count","Fig 29 - Freq. Dist. of Self Defined Ethnicity","Analysis of Stop and Search 2017-20")
self_defined_ethnicity_count_dist

self_defined_ethnicity_count_percent_dist <- count_geom_bar_percent_noTheme(self_defined_ethnicity_count,self_defined_ethnicity_count$Self.defined.ethnicity,self_defined_ethnicity_count$pct,"Self Defined Ethnicity","Count","Fig 30 - Percentage Dist. of Self Defined Ethnicity","Analysis of Stop and Search 2017-20")
self_defined_ethnicity_count_percent_dist



#Outcome based count
outcome_count<- stop_and_search %>% group_by(Outcome) %>% dplyr::summarise(count=n()) %>%mutate(pct = count/sum(count))

outcome_count_dist <- count_geom_bar(outcome_count,outcome_count$Outcome,outcome_count$count,"Type of Outcome","Count","Fig 31 - Freq. Dist. of Type of Outcome","Analysis of Stop and Search 2017-20")
outcome_count_dist

outcome_count_percent_dist <- count_geom_bar_percent_noTheme(outcome_count,outcome_count$Outcome,outcome_count$pct,"Type of Outcome","Percent","Fig 32 - Percentage Dist. of Type of Outcome","Analysis of Stop and Search 2017-20")
outcome_count_percent_dist



#Object.of.search
object_of_search_count <- stop_and_search %>% group_by(Object.of.search) %>% dplyr::summarise(count=n())%>%mutate(pct = count/sum(count))

object_of_search_count_dist <- count_geom_bar(object_of_search_count,object_of_search_count$Object.of.search,object_of_search_count$count,"Object of Search","Count","Fig 33 - Freq. Dist. of Object of Search","Analysis of Stop and Search 2017-20")
object_of_search_count_dist

object_of_search_count_dist <- count_geom_bar_percent_noTheme(object_of_search_count,object_of_search_count$Object.of.search,object_of_search_count$pct,"Object of Search","Percent","Fig 34 - Percentage Dist. of Type of Object of Search","Analysis of Stop and Search 2017-20")
object_of_search_count_dist



#Gender based outcome 

gender_outcome_count<- stop_and_search %>% group_by(Gender,Outcome) %>% dplyr::summarise(count=n())

gender_outcome_count_dist <- count_geom_bar_dodge_3(gender_outcome_count,gender_outcome_count$Outcome,gender_outcome_count$Gender, gender_outcome_count$count,"Type of Outcome","Count","Fig 35 - Freq. Dist. of Outcome based on Gender","Analysis of Stop and Search 2017-20")
gender_outcome_count_dist


gender_outcome_count_dist_grid <- count_geom_bar_grid_count_2(gender_outcome_count,gender_outcome_count$Outcome,gender_outcome_count$Gender, gender_outcome_count$count,"Type of Outcome","Count","Fig 36 - Freq. Dist. of Outcome based on Gender","Analysis of Stop and Search 2017-20")
gender_outcome_count_dist_grid


gender_outcome_df_percentage<- stop_and_search %>% group_by(Gender,Outcome) %>% dplyr::summarise(count=n())

gender_outcome_df_percentage<-gender_outcome_df_percentage[!(is.na(gender_outcome_df_percentage$Outcome) | gender_outcome_df_percentage$Outcome==""), ]
gender_outcome_df_percentage<-gender_outcome_df_percentage[!(is.na(gender_outcome_df_percentage$Gender) | gender_outcome_df_percentage$Gender==""), ]

gender_outcome_df_percentage$Gender<- factor(gender_outcome_df_percentage$Gender)


gender_outcome_df_percentage_Genderbased<- find_percentage_dataset(gender_outcome_df_percentage,"Gender","Outcome")
gender_outcome_df_percentage_Outcomebased<- find_percentage_dataset(gender_outcome_df_percentage,"Outcome","Gender")

gender_outcome_df_percentage_Genderbased_bar <- count_geom_bar_grid_count_percentage_withTheme(gender_outcome_df_percentage_Genderbased,gender_outcome_df_percentage_Genderbased$Gender,gender_outcome_df_percentage_Genderbased$Outcome,gender_outcome_df_percentage_Genderbased$Percentage,"Gender","Percent","Fig 37 - Outcome Percentage Distribution based on Gender","Crime Data Analysis during 2017-20 - City of London")
gender_outcome_df_percentage_Genderbased_bar

gender_outcome_df_percentage_Genderbased_bar2 <- count_geom_bar_dodge_percent_invert_3(gender_outcome_df_percentage_Genderbased,gender_outcome_df_percentage_Genderbased$Percentage,gender_outcome_df_percentage_Genderbased$Gender, gender_outcome_df_percentage_Genderbased$Outcome,"Count","Type of Outcome","fig 38 - Perct. Dist. of Outcome based on Gender","Analysis of Stop and Search 2017-20")
gender_outcome_df_percentage_Genderbased_bar2

gender_outcome_df_percentage_Outcomebased_bar3 <- count_geom_bar_dodge_percent_invert_3(gender_outcome_df_percentage_Outcomebased,gender_outcome_df_percentage_Outcomebased$Percentage,gender_outcome_df_percentage_Outcomebased$Gender, gender_outcome_df_percentage_Outcomebased$Outcome,"Percent","Type of Outcome","fig 39 - Gender based Percnt. Dist. Outcome","Analysis of Stop and Search 2017-20")
gender_outcome_df_percentage_Outcomebased_bar3



# crime in each month as percentage of total crime in that year 



year_month_df_percentage<- find_percentage_dataset(year_month_df_percentage,"year","month")
# These two lines wors but changes the order of graph
#year_month_df_percentage$month<-month.abb[as.numeric(year_month_df_percentage$month)]
year_month_df_percentage$month<-as.factor(year_month_df_percentage$month) 
year_month_df_percentage_bar <- count_geom_bar_grid_count_percentage(year_month_df_percentage,year_month_df_percentage$year,year_month_df_percentage$month,year_month_df_percentage$Percentage,"Year","Percent","Fig 12 - Different Crime type Per year Percentage Distribution","Crime Data Analysis during 2017-20 - City of London")
year_month_df_percentage_bar







# Age Range - Ethnicity Analysis
age_ethnicity_count<- stop_and_search %>% group_by(Age.range,Officer.defined.ethnicity) %>% dplyr::summarise(count=n())

age_ethnicity_count_dist <- count_geom_bar_dodge_3(age_ethnicity_count,age_ethnicity_count$Age.range,age_ethnicity_count$Officer.defined.ethnicity, age_ethnicity_count$count,"Age Range","Count","Fig 40 - Freq. Dist. of Outcome based on Ethnicity","Analysis of Stop and Search 2017-20")
age_ethnicity_count_dist

age_ethnicity_count_dist2 <- count_geom_bar_dodge_3(age_ethnicity_count,age_ethnicity_count$Officer.defined.ethnicity,age_ethnicity_count$Age.range, age_ethnicity_count$count,"Officer Defined Ethnicity","Count","Fig - 41 Freq. Dist. of Ethnicity based on Age Range","Analysis of Stop and Search 2017-20")
age_ethnicity_count_dist2

age_ethnicity_count_dist3 <- count_geom_bar_grid_count_2(age_ethnicity_count,age_ethnicity_count$Officer.defined.ethnicity,age_ethnicity_count$Age.range, age_ethnicity_count$count,"Age Range","Count","Fig 42 - Freq. Dist. of Age Range based on Ethnicity","Analysis of Stop and Search 2017-20")
age_ethnicity_count_dist3

age_ethnicity_count_dist4 <- count_geom_bar_grid_count_2(age_ethnicity_count,age_ethnicity_count$Age.range,age_ethnicity_count$Officer.defined.ethnicity, age_ethnicity_count$count,"Age Range","Count","Fig 43 - Freq. Dist. of Ethnicity based on Gender","Analysis of Stop and Search 2017-20")
age_ethnicity_count_dist4

age_ethnicity_percentage<- stop_and_search %>% group_by(Age.range,Officer.defined.ethnicity) %>% dplyr::summarise(count=n())

age_ethnicity_percentage<-age_ethnicity_percentage[!(is.na(age_ethnicity_percentage$Officer.defined.ethnicity) | age_ethnicity_percentage$Officer.defined.ethnicity==""), ]
age_ethnicity_percentage<-age_ethnicity_percentage[!(is.na(age_ethnicity_percentage$Age.range) | age_ethnicity_percentage$Age.range==""), ]

age_ethnicity_percentage$Age.range<- factor(age_ethnicity_percentage$Age.range)
age_ethnicity_percentage$Officer.defined.ethnicity<- factor(age_ethnicity_percentage$Officer.defined.ethnicity)


age_ethnicity_percentage_Agebased<- find_percentage_dataset(age_ethnicity_percentage,"Age.range","Officer.defined.ethnicity")
age_ethnicity_percentage_Ethnicitybased<- find_percentage_dataset(age_ethnicity_percentage,"Officer.defined.ethnicity","Age.range")

age_ethnicity_percentage_Agebased_bar <- count_geom_bar_grid_count_percentage(age_ethnicity_percentage_Agebased,age_ethnicity_percentage_Agebased$Officer.defined.ethnicity,age_ethnicity_percentage_Agebased$Age.range,age_ethnicity_percentage_Agebased$Percentage,"Age Range","Percent","Fig 44 - Percentage Distribution Ethnicity based on Age Range","Crime Data Analysis during 2017-20 - City of London")
age_ethnicity_percentage_Agebased_bar

age_ethnicity_percentage_Agebased_bar2 <- count_geom_bar_dodge_percent_invert_3(age_ethnicity_percentage_Agebased,age_ethnicity_percentage_Agebased$Percentage,age_ethnicity_percentage_Agebased$Age.range, age_ethnicity_percentage_Agebased$Officer.defined.ethnicity,"Percent","Offficer Defined Ethnicity","Fig 45 - Percentage Distribution Ethnicity based on Age Range","Analysis of Stop and Search 2017-20")
age_ethnicity_percentage_Agebased_bar2

age_ethnicity_percentage_Ethnicitybased_bar2 <- count_geom_bar_dodge_percent_invert_3(age_ethnicity_percentage_Ethnicitybased,age_ethnicity_percentage_Ethnicitybased$Percentage,age_ethnicity_percentage_Ethnicitybased$Officer.defined.ethnicity, age_ethnicity_percentage_Ethnicitybased$Age.range,"Percent","Age Range","Fig 46 - Perct Dist. of Age Range based on Ethnicity","Analysis of Stop and Search 2017-20")
age_ethnicity_percentage_Ethnicitybased_bar2



# Changes in Crime during the period(Annually)
Gender_outcome_table=table(stop_and_search$Outcome,stop_and_search$Gender)
options(ztable.type="html")
z=ztable(Gender_outcome_table) 
z %>% 
  makeHeatmap(palette="Blues") %>%
  print(type = "viewer",caption="Heatmap 7. Freq Dist of Crime Type based on Gender")






#Gender-Age based Stop and Search 
gender_age<- stop_and_search %>% group_by(Gender,Age.range) %>% dplyr::summarise(count=n())

gender_age<-gender_age[!(is.na(gender_age$Gender) | gender_age$Gender==""), ]
gender_age<-gender_age[!(is.na(gender_age$Age.range) | gender_age$Age.range==""), ]

gender_age_count_dist <- count_geom_bar_dodge_3(gender_age,gender_age$Age.range,gender_age$Gender, gender_age$count,"Age Range","Count","Fig 47 - Freq. Dist. of Age Range based on Gender","Analysis of Stop and Search 2017-20")
gender_age_count_dist

gender_age_count_dist2 <- count_geom_bar_dodge_3(gender_age,gender_age$Gender,gender_age$Age.range, gender_age$count,"Gender","Count","Fig 48 - Freq. Dist. of Gender based on Age Range","Analysis of Stop and Search 2017-20")
gender_age_count_dist2

gender_age_count_dist3 <- count_geom_bar_grid_count_2(gender_age,gender_age$Gender,gender_age$Age.range, gender_age$count,"Gender","Count","Fig 49 - Freq. Dist. of Age Range based on Gender","Analysis of Stop and Search 2017-20")
gender_age_count_dist3

gender_age_count_dist4 <- count_geom_bar_grid_count_2(gender_age,gender_age$Age.range,gender_age$Gender, gender_age$count,"Age Range","Count","Fig 50 - Freq. Dist. of Gender based on Age Range","Analysis of Stop and Search 2017-20")
gender_age_count_dist4


gender_age_percentage<- stop_and_search %>% group_by(Gender,Age.range) %>% dplyr::summarise(count=n())

gender_age_percentage<-gender_age_percentage[!(is.na(gender_age_percentage$Gender) | gender_age_percentage$Gender==""), ]
gender_age_percentage<-gender_age_percentage[!(is.na(gender_age_percentage$Age.range) | gender_age_percentage$Age.range==""), ]

gender_age_percentage$Age.range<- factor(gender_age_percentage$Age.range)
gender_age_percentage$Gender<- factor(gender_age_percentage$Gender)


gender_age_percentage_Agebased<- find_percentage_dataset(gender_age_percentage,"Age.range","Gender")
gender_age_percentage_Genderbased<- find_percentage_dataset(gender_age_percentage,"Gender","Age.range")

gender_age_percentage_Agebased_bar <- count_geom_bar_grid_count_percentage(gender_age_percentage_Agebased,gender_age_percentage_Agebased$Gender,gender_age_percentage_Agebased$Age.range,gender_age_percentage_Agebased$Percentage,"Age Range","Percent","Fig 51 - Gender Percentage Distribution - Based on Age Range","Crime Data Analysis during 2017-20 - City of London")
gender_age_percentage_Agebased_bar

age_ethnicity_percentage_Agebased_bar2 <- count_geom_bar_dodge_percent_invert_3(gender_age_percentage_Agebased,gender_age_percentage_Agebased$Percentage,gender_age_percentage_Agebased$Age.range, gender_age_percentage_Agebased$Gender,"Percent","Gender","Fig 52 - Gender Percentage Distribution - Based on Age Range","Analysis of Stop and Search 2017-20")
age_ethnicity_percentage_Agebased_bar2

gender_age_percentage_Genderbased_bar <- count_geom_bar_dodge_percent_invert_3(gender_age_percentage_Genderbased,gender_age_percentage_Genderbased$Percentage,gender_age_percentage_Genderbased$Gender, gender_age_percentage_Genderbased$Age.range,"Percent","Age Range","Fig 53 - Age Range Percentage Distribution - Based on Gender","Analysis of Stop and Search 2017-20")
gender_age_percentage_Genderbased_bar




#Gender based outcome 

gender_Ethnicity_count<- stop_and_search %>% group_by(Gender,Officer.defined.ethnicity) %>% dplyr::summarise(count=n())

gender_Ethnicity_count<-gender_Ethnicity_count[!(is.na(gender_Ethnicity_count$Officer.defined.ethnicity) | gender_Ethnicity_count$Officer.defined.ethnicity==""), ]
gender_Ethnicity_count<-gender_Ethnicity_count[!(is.na(gender_Ethnicity_count$Gender) | gender_Ethnicity_count$Gender==""), ]

gender_Ethnicity_count$Gender<- factor(gender_Ethnicity_count$Gender)
gender_Ethnicity_count$Officer.defined.ethnicity<- factor(gender_Ethnicity_count$Officer.defined.ethnicity)


gender_Ethnicity_percentage_Genderbased<- find_percentage_dataset(gender_Ethnicity_count,"Gender","Officer.defined.ethnicity")
gender_Ethnicity_percentage_Ethbased<- find_percentage_dataset(gender_Ethnicity_count,"Officer.defined.ethnicity","Gender")


gender_Eth_percentage_Genderbased_bar2 <- count_geom_bar_dodge_percent_invert_3(gender_Ethnicity_percentage_Genderbased,gender_Ethnicity_percentage_Genderbased$Percentage,gender_Ethnicity_percentage_Genderbased$Gender, gender_Ethnicity_percentage_Genderbased$Officer.defined.ethnicity,"Count","Ethnicity","fig 54 - Perct. Dist. of Ethnicity based on Gender Prop.","Analysis of Stop and Search 2017-20")
gender_Eth_percentage_Genderbased_bar2

eth_gender_percentage_Ethbased_bar3 <- count_geom_bar_dodge_percent_invert_3(gender_Ethnicity_percentage_Ethbased,gender_Ethnicity_percentage_Ethbased$Percentage,gender_Ethnicity_percentage_Ethbased$Gender, gender_Ethnicity_percentage_Ethbased$Officer.defined.ethnicity,"Percent","Ethnicity","fig 55 - Gender based Percnt. Dist. Ethnicity","Analysis of Stop and Search 2017-20")
eth_gender_percentage_Ethbased_bar3




#Ethnicit - outcome 

outcome_Ethnicity_count<- stop_and_search %>% group_by(Outcome,Officer.defined.ethnicity) %>% dplyr::summarise(count=n())

outcome_Ethnicity_count<-outcome_Ethnicity_count[!(is.na(outcome_Ethnicity_count$Officer.defined.ethnicity) | outcome_Ethnicity_count$Officer.defined.ethnicity==""), ]
outcome_Ethnicity_count<-outcome_Ethnicity_count[!(is.na(outcome_Ethnicity_count$Outcome) | outcome_Ethnicity_count$Outcome==""), ]

outcome_Ethnicity_count$Outcome<- factor(outcome_Ethnicity_count$Outcome)
outcome_Ethnicity_count$Officer.defined.ethnicity<- factor(outcome_Ethnicity_count$Officer.defined.ethnicity)


outcome_Eth_percentage_Outcomebased<- find_percentage_dataset(outcome_Ethnicity_count,"Outcome","Officer.defined.ethnicity")
outcome_Eth_percentage_Ethbased<- find_percentage_dataset(outcome_Ethnicity_count,"Officer.defined.ethnicity","Outcome")


outcome_Eth_perc_Outcomebased_bar <- count_geom_bar_dodge_percent_invert_3(outcome_Eth_percentage_Outcomebased,outcome_Eth_percentage_Outcomebased$Percentage,outcome_Eth_percentage_Outcomebased$Officer.defined.ethnicity, outcome_Eth_percentage_Outcomebased$Outcome,"Count","Type of Outcome","fig 56 - Perct. Dist. of Ethnicity based on Outcome","Analysis of Stop and Search 2017-20")
outcome_Eth_perc_Outcomebased_bar

outcome_Eth_perc_Ethbased_bar <- count_geom_bar_dodge_percent_invert_3(outcome_Eth_percentage_Ethbased,outcome_Eth_percentage_Ethbased$Percentage,outcome_Eth_percentage_Ethbased$Officer.defined.ethnicity, outcome_Eth_percentage_Ethbased$Outcome,"Percent","Type of Outcome","fig 57 - Outcome based Percnt. Dist. of Ethnicity","Analysis of Stop and Search 2017-20")
outcome_Eth_perc_Ethbased_bar



#Age - outcome 

outcome_Age_count<- stop_and_search %>% group_by(Outcome,Age.range) %>% dplyr::summarise(count=n())

outcome_Age_count<-outcome_Age_count[!(is.na(outcome_Age_count$Age.range) | outcome_Age_count$Age.range==""), ]
outcome_Age_count<-outcome_Age_count[!(is.na(outcome_Age_count$Outcome) | outcome_Age_count$Outcome==""), ]

outcome_Age_count$Outcome<- factor(outcome_Age_count$Outcome)
outcome_Age_count$Age.range<- factor(outcome_Age_count$Age.range)


outcome_Age_percentage_Outcomebased<- find_percentage_dataset(outcome_Age_count,"Outcome","Age.range")
outcome_Age_percentage_Agebased<- find_percentage_dataset(outcome_Age_count,"Age.range","Outcome")


outcome_Age_perc_Outcomebased_bar <- count_geom_bar_dodge_percent_invert_3(outcome_Age_percentage_Outcomebased,outcome_Age_percentage_Outcomebased$Percentage,outcome_Age_percentage_Outcomebased$Age.range, outcome_Age_percentage_Outcomebased$Outcome,"Count","Type of Outcome","fig 58 - Perct. Dist. of Outcome based on Age","Analysis of Stop and Search 2017-20")
outcome_Age_perc_Outcomebased_bar

outcome_Age_perc_Agebased_bar <- count_geom_bar_dodge_percent_invert_3(outcome_Age_percentage_Agebased,outcome_Age_percentage_Agebased$Percentage,outcome_Age_percentage_Agebased$Age.range, outcome_Age_percentage_Agebased$Outcome,"Percent","Type of Outcome","fig 59 - Outcome based Percnt. Dist. of Age","Analysis of Stop and Search 2017-20")
outcome_Age_perc_Agebased_bar






#----------------------------------------------------------------
# Outcome Data


outcome_count<- outcome %>% group_by(Outcome.type) %>% dplyr::summarise(count=n()) %>%mutate(pct = count/sum(count))


outcome_count_percent_dist <- count_geom_bar_percent(outcome_count,outcome_count$Outcome.type,outcome_count$pct,"Outcome Type","Percent"," Percentage Dist. of Outcome Type","Analysis of Stop and Search 2017-20")
outcome_count_percent_dist



outcome2017 = subset(outcome, year=='2017')
outcome2018 = subset(outcome, year=='2018')
outcome2019 = subset(outcome, year=='2019')
outcome2020 = subset(outcome, year=='2020')


# Heat Maps and Pivot Tables using Ztable 

y=table(outcome$year,outcome$month)

options(ztable.type="html")
z=ztable(y) 
z %>% makeHeatmap() %>% print(type = "viewer",caption="HeatMap 1.  Heatmap Table")

z %>% 
  makeHeatmap(palette="Blues") %>%
  print(type = "viewer",caption="HeatMap 1. Outcome Count - Every Month Per Year ")




#crime2017$month <- month.abb[as.numeric(crime2017$month)]

heatmap_2017=table(outcome2017$Outcome.type,outcome2017$month)
options(ztable.type="html")
heatmap_2017z=ztable(heatmap_2017) 
heatmap_2017z %>% makeHeatmap() %>% print(type = "viewer",caption="Heat Map 2 Freq. Dist. of each Outcome Type (year 2017)")


#crime2018$month <- month.abb[as.numeric(crime2018$month)]

heatmap_2018=table(outcome2018$Outcome.type,outcome2018$month)
options(ztable.type="html")
heatmap_2018z=ztable(heatmap_2018) 
heatmap_2018z %>% makeHeatmap() %>% print(type = "viewer",caption="Heat Map 3 Freq. Dist. of each Outcome Type (year 2018)")

heatmap_2019=table(outcome2019$Outcome.type,outcome2019$month)
options(ztable.type="html")
heatmap_2019z=ztable(heatmap_2019) 
heatmap_2019z %>% makeHeatmap() %>% print(type = "viewer",caption="Heat Map 4 Freq. Dist. of each Outcome Type (year 2019)")

x=table(outcome2020$Outcome.type,outcome2020$month)
options(ztable.type="html")
z=ztable(x) 
z %>% makeHeatmap() %>% print(type = "viewer",caption="Heat Map 5. Freq. Dist. of each Crime Type (year 2020)")

x=table(outcome$Outcome.type,outcome$year)
options(ztable.type="html")
z=ztable(x) 
z %>% makeHeatmap() %>% print(type = "viewer",caption="Heat Map 6 Freq. Dist. of each Outcome Type per year (2017-2020)")







#######-----------------------------------------------------------
# Functions used

find_percentage_dataset<- function(dataset,var1,var2){
  var.level <- dataset[[var1]]
  dataset<- dataset %>%
    pivot_wider(names_from = var1, values_from = c(count))
  # fill Na with zero
  dataset[is.na(dataset)] <- 0
  for(i in 1:length(var.level)){
    p <-var.level[i]
    g = dataset[[p]]/sum(dataset[[p]])
    dataset[[p]] <- g
  }
  
  outcome_wider <- melt(setDT(dataset), id.vars = var2, variable.name = var1)
  names(outcome_wider)[names(outcome_wider) == "value"] <- "Percentage"
  outcome_wider$Percentage<- round(outcome_wider$Percentage,2) 
  return(outcome_wider)
}

# Complete Percentage Function

add_percent_columns <- function(dataset,var1,var2) {
  #Remove the Blanks - to avoid error
  dataset<-dataset[!(is.na(var1) | var1==""), ]
  dataset<-dataset[!(is.na(var2) | var2==""), ]
  dataset[[var1]] <- factor(dataset[[var1]])
  dataset[[var2]] <- factor(dataset[[var2]])
  
  var.level <- dataset[[var1]]
  
  dataset<- dataset %>%
    pivot_wider(names_from = dataset[[var1]], values_from = c(count))
  # fill Na with zero
  dataset[is.na(dataset)] <- 0
  
  for(i in 1:length(var.level)){
    p <-var.level[i]
    g = dataset[[p]]/sum(dataset[[p]])*100
    dataset[[p]] <- g
  }
  
  outcome_wider <- melt(setDT(dataset), id.vars = var2, variable.name = var1)
  names(outcome_wider)[names(outcome_wider) == "value"] <- "Percentage"
  outcome_wider$Percentage<- round(outcome_wider$Percentage,2) 
  return(outcome_wider)
  
}









count_geom_bar_dodge_3 <- function(dataset,var1,var2,var3,lab1,lab2,title1,caption1) {
  bar1 <- ggplot(dataset,aes(var1,var3,fill=factor(var2)))+
    geom_bar(stat="identity",position = "dodge")+
    theme(axis.text.x = element_text(angle = 90, hjust = 0))+
    labs(x=lab1,y=lab2,
         title=title1,
         caption=caption1)+
    geom_text(aes(label= var3),position = position_dodge(width=1),vjust = -0.25,color="black")
  return(bar1)
}

count_geom_bar_dodge_percent_3 <- function(dataset,var1,var2,var3,lab1,lab2,title1,caption1) {
  bar1 <- ggplot(dataset,aes(var1,var3,fill=factor(var2)))+
    geom_bar(stat="identity",position = "dodge")+
    theme(axis.text.x = element_text(angle = 90, hjust = 0))+
    labs(x=lab1,y=lab2,
         title=title1,
         caption=caption1)+
    geom_text(aes(label=scales::percent(var3,accuracy=.11)),position = position_dodge(width=1),vjust = 0,color="black")
  return(bar1)
}

count_geom_bar_dodge_percent_invert_3 <- function(dataset,var1,var2,var3,lab1,lab2,title1,caption1) {
  bar1 <- ggplot(dataset,aes(var1,var3,fill=factor(var2)))+
    geom_bar(stat="identity",position = "dodge")+
    theme(axis.text.x = element_text(angle = 90))+
    labs(x=lab1,y=lab2,
         title=title1,
         caption=caption1)+
    geom_text(aes(label=scales::percent(var1,accuracy=.11)),position = position_dodge(width=1),hjust = -0.5,color="black",cex =4)+
    scale_x_continuous(labels = scales::percent)
  return(bar1)
}




#######################################



#percent count frequency GeomBAR
count_geom_bar_percent <- function(dataset,var1,var2,lab1,lab2,title1,caption1) {
  bar1 <- ggplot(dataset,aes(var1,var2,fill=factor(var1)))+
    geom_bar(stat="identity")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    labs(x=lab1,y=lab2,
         title=title1,
         caption=caption1)+
    scale_y_continuous(labels = scales::percent)+
    geom_text(aes(label=scales::percent(var2,accuracy=.11)),vjust = 0,color="black")
  return(bar1)
}

count_geom_bar_percent_noTheme <- function(dataset,var1,var2,lab1,lab2,title1,caption1) {
  bar1 <- ggplot(dataset,aes(var1,var2,fill=factor(var1)))+
    geom_bar(stat="identity")+
    labs(x=lab1,y=lab2,
         title=title1,
         caption=caption1)+
    geom_text(aes(label=scales::percent(var2,accuracy=.11)),vjust = 0,color="black")
  return(bar1)
}


count_geom_bar_percent_vert <- function(dataset,var1,var2,lab1,lab2,title1,caption1) {
  bar1 <- ggplot(dataset,aes(var1,var2,fill=factor(var2)))+
    geom_bar(stat="identity")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    labs(x=lab1,y=lab2,
         title=title1,
         caption=caption1)+
    scale_x_continuous(labels = scales::percent)+
    geom_text(aes(label=scales::percent(var1,accuracy=.11)),hjust = -0,color="black")
  return(bar1)
}

#########################




#percent count frequency GeomBAR
count_geom_bar <- function(dataset,var1,var2,lab1,lab2,title1,caption1) {
  bar1 <- ggplot(dataset,aes(var1,var2,fill=factor(var1)))+
    geom_bar(stat="identity")+
    labs(x=lab1,y=lab2,
         title=title1,
         caption=caption1)+
    geom_text(aes(label= var2),vjust = 0,color="black")
  return(bar1)
}

count_geom_bar2 <- function(dataset,var1,var2,lab1,lab2,title1,caption1) {
  bar1 <- ggplot(dataset,aes(var1,var2,fill=factor(var2)))+
    geom_bar(stat="identity")+
    labs(x=lab1,y=lab2,
         title=title1,
         caption=caption1)+
    geom_text(aes(label= var1),hjust = -1,color="black")
  return(bar1)
}

count_geom_bar_withTheme <- function(dataset,var1,var2,lab1,lab2,title1,caption1) {
  bar1 <- ggplot(dataset,aes(var1,var2,fill=var1))+
    geom_bar(stat="identity")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    labs(x=lab1,y=lab2,
         title=title1,
         caption=caption1)+
    geom_text(aes(label= var2),vjust = 0,color="black")
  return(bar1)
}


#################################



# Bar Chart Three variable with text at 90deg vertical 
count_geom_bar_count_dodge <- function(dataset,var1,var2,var3,lab1,lab2,title1,caption1) {
  bar1 <- ggplot(dataset,aes(var1,var3,fill=var2))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    geom_bar(stat="identity",position = "dodge")+
    labs(x=lab1,y=lab2,
         title=title1,
         caption=caption1)+
    geom_text(aes(y=var3+0.25,label = count ),position = position_dodge(width=1),hjust = 0,cex= 4,angle = 90,color ="black")+
    # geom_text(aes(label=var3), position=position_dodge(width=0.9), vjust=-0.25)
    return(bar1)
}



########################################



# Bar Chart two variable and one fill variable with text at 90deg vertical 
count_geom_bar_percent_dodge <- function(dataset,var1,var2,var3,lab1,lab2,title1,caption1) {
  bar1 <- ggplot(dataset,aes(var3,var1,fill=factor(var2)))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    geom_bar(stat="identity",position = "dodge")+
    # facet_wrap(var1)+
    labs(x=lab1,y=lab2,
         title=title1,
         caption=caption1)+
    # geom_text(aes(label= scales::percent(var3,accuracy = .11),position=position_dodge(width=0.9)),vjust = 0,color="black")
    geom_text(aes(label=scales::percent(var3,accuracy = .11)), position=position_dodge(width=0.9), hjust= -1,cex = 3)
  return(bar1)
}






##########################################


# Geom Bar Grid count
count_geom_bar_grid_count <- function(dataset,var1,var2,var3,lab1,lab2,title1,caption1) {
  bar1 <- ggplot(dataset,aes(var2,var3,fill=factor(var2)))+
    geom_bar(stat="identity")+
    facet_wrap(var1)+
    labs(x=lab1,y=lab2,
         title=title1,
         caption=caption1)+
    geom_text(aes(label= var3),vjust = 0,color="black")
  return(bar1)
}

count_geom_bar_grid_count_2 <- function(dataset,var1,var2,var3,lab1,lab2,title1,caption1) {
  bar1 <- ggplot(dataset,aes(var1,var3,fill=factor(var1)))+
    geom_bar(stat="identity")+
    facet_wrap(var2)+
    labs(x=lab1,y=lab2,
         title=title1,
         caption=caption1)+
    geom_text(aes(label= var3),vjust = 0,color="black")
  return(bar1)
}

count_geom_bar_grid_count_percentage <- function(dataset,var1,var2,var3,lab1,lab2,title1,caption1) {
  bar1 <- ggplot(dataset,aes(var2,var3,fill=factor(var2)))+
    geom_bar(stat="identity")+
    facet_wrap(var1)+
    labs(x=lab1,y=lab2,
         title=title1,
         caption=caption1)+
    scale_y_continuous(labels = scales::percent)+
    geom_text(aes(label= scales::percent(var3,accuracy = .11)),vjust = 0,color="black")
  return(bar1)
}

count_geom_bar_grid_count_percentage_withTheme <- function(dataset,var1,var2,var3,lab1,lab2,title1,caption1) {
  bar1 <- ggplot(dataset,aes(var2,var3,fill=factor(var2)))+
    geom_bar(stat="identity")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    facet_wrap(var1)+
    labs(x=lab1,y=lab2,
         title=title1,
         caption=caption1)+
    scale_y_continuous(labels = scales::percent)+
    geom_text(aes(label= scales::percent(var3,accuracy = .11)),vjust = 0,color="black",cex = 4)
  return(bar1)
}

# Geom Line Funtion

count_geom_line_fwrap <- function(dataset,var1,var2,var3,lab1,lab2,title1,caption1) {
  bar1 <- ggplot(dataset,aes(var1,var2,fill=factor(var3),group = 1))+
    geom_line()+
    facet_wrap(var3)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    labs(x=lab1,y=lab2,
         title=title1,
         caption=caption1)
  #   scale_x_discrete(limits=month.abb)
  return(bar1)
}




count_geom_line_fwrap2 <- function(dataset,var1,var2,var3,var4,lab1,lab2,title1,caption1) {
  bar1 <- ggplot(dataset)+
    geom_line(aes(var2,var3,color=factor(var1)))+
    facet_wrap(var4)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    labs(x=lab1,y=lab2,
         title=title1,
         caption=caption1)+
    scale_x_discrete(limits=month.abb)
  return(bar1)
} 


count_geom_col_fwrap <- function(dataset,var1,var2,var3,var4,lab1,lab2,title1,caption1) {
  bar1 <- ggplot(dataset,aes(var2,var3, fill=factor(var1)))+
    geom_line()+
    facet_wrap(var4)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    labs(x=lab1,y=lab2,
         title=title1,
         caption=caption1)+
    scale_x_discrete(limits=month.abb)
  return(bar1)
} 

count_geom_col_fwrap2 <- function(dataset,var1,var2,var3,var4,lab1,lab2,title1,caption1) {
  bar1 <- ggplot(dataset)+
    geom_col(aes(var2,var3, fill=factor(var1)))+
    facet_wrap(var4)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    labs(x=lab1,y=lab2,
         title=title1,
         caption=caption1)+
    scale_x_discrete(limits=month.abb)
  return(bar1)
} 






map_for_london_per_year <- function(dataset1,dataset2, title1) {
  numCrimesByLSOA<-dataset1 %>%
    select(LSOA.code, LSOA.name, Crime.type) %>%
    group_by(LSOA.code) %>%
    dplyr::summarise(Num.crimes=n()) 
  print(numCrimesByLSOA)
  londonShape@data<-left_join(londonShape@data, numCrimesByLSOA,
                              by=c('code'='LSOA.code'))
  tmap_mode("view")
  tm_shape(londonShape) +
    tm_fill("Num.crimes", alpha=0.5, style="kmeans", border.col = "black") +
    tm_borders(alpha=0.5)
}



# --------------------------------------------------------------------
# Usiing Random Forest/Naive Bayes/MLR to Predict Gender, Age and Ethnicity in Crime Data





library(psych)


#---------------------------------------------------------------
#Further Classification of data

new_ss <- subset(stop_and_search, select = -c(Removal.of.more.than.just.outer.clothing,Legislation,Policing.operation,Part.of.a.policing.operation,Outcome.linked.to.object.of.search) )

levels(new_ss$Object.of.search)
levels(new_ss$Object.of.search) <- c(levels(new_ss$Object.of.search), "Drugs","Social Disorder","Theft","Criminal Activity", "Other Crime", "Weapons")
new_ss$Object.of.search[new_ss$Object.of.search == "Fireworks"] <- "Social Disorder"
new_ss$Object.of.search[new_ss$Object.of.search == "Controlled drugs"] <- "Drugs"
new_ss$Object.of.search[new_ss$Object.of.search == "Controlled drugs"] <- "Theft"
new_ss$Object.of.search[new_ss$Object.of.search == "Article for use in theft"] <- "Theft"
new_ss$Object.of.search[new_ss$Object.of.search == "Evidence of offences under the Act"] <- "Other Crime"
new_ss$Object.of.search[new_ss$Object.of.search == "Anything to threaten or harm anyone"] <- "Weapons"
new_ss$Object.of.search[new_ss$Object.of.search == "Crossbows"] <- "Weapons"
new_ss$Object.of.search[new_ss$Object.of.search == "Stolen goods"] <- "Theft"
new_ss$Object.of.search[new_ss$Object.of.search == "Psychoactive substances"] <- "Drugs"
new_ss$Object.of.search[new_ss$Object.of.search == "Offensive weapons"] <- "Weapons"
new_ss$Object.of.search[new_ss$Object.of.search == "Firearms"] <- "Weapons"
new_ss$Object.of.search[new_ss$Object.of.search == "Anything to threaten or harm anyone"] <- "Weapons"
new_ss$Object.of.search[new_ss$Object.of.search == "Articles for use in criminal damage"] <- "Criminal Activity"


summary(new_ss)
levels(new_ss$Outcome)
levels(new_ss$Object.of.search)
levels(new_ss$Outcome) <- c(levels(new_ss$Outcome), "Pending","Guilty","Not Guilty","Others")

new_ss$Outcome[new_ss$Outcome == "A no further action disposal"] <- "Pending"
new_ss$Outcome[new_ss$Outcome == "Community resolution"] <- "Guilty"
new_ss$Outcome[new_ss$Outcome == "Suspect arrested"] <- "Guilty"
new_ss$Outcome[new_ss$Outcome == "Arrest"] <- "Guilty"
new_ss$Outcome[new_ss$Outcome == "Penalty Notice for Disorder"] <- "Guilty"
new_ss$Outcome[new_ss$Outcome == "Khat or Cannabis warning"] <- "Guilty"
new_ss$Outcome[new_ss$Outcome == "Nothing found - no further action"] <- "Not Guilty"
new_ss$Outcome[new_ss$Outcome == "Local resolution"] <- "Others"
new_ss$Outcome[new_ss$Outcome == "Caution (simple or conditional)"] <- "Guilty"

new_ss$Outcome[new_ss$Outcome == "Suspect summonsed to court"] <- "Pending"
new_ss$Outcome[new_ss$Outcome == "Nothing found - no further action"] <- "Not Guilty"
new_ss$Outcome[new_ss$Outcome == "Offender given drugs possession warning"] <- "Guilty"
new_ss$Outcome[new_ss$Outcome == "Offender given penalty notice"] <- "Guilty"
new_ss$Outcome[new_ss$Outcome == "Penalty Notice for Disorder"] <- "Guilty"
new_ss$Outcome[new_ss$Outcome == "Summons / charged by post"] <- "Pending"
new_ss$Outcome[new_ss$Outcome == "Suspect arrested"] <- "Guilty"
new_ss$Outcome[new_ss$Outcome == "Suspect summonsed to court"] <- "Pending"

#-------------------------------------------------------------------
#Removal of blanks and NA

new_ss<-new_ss[!(is.na(new_ss$Object.of.search) | new_ss$Object.of.search==""), ]
new_ss<-new_ss[!(is.na(new_ss$Officer.defined.ethnicity) | new_ss$Officer.defined.ethnicity==""), ]
new_ss<-new_ss[!(is.na(new_ss$Gender) | new_ss$Gender==""), ]
new_ss<-new_ss[!(is.na(new_ss$Age.range) | new_ss$Age.range==""), ]
new_ss<-new_ss[!(is.na(new_ss$Latitude) | new_ss$Latitude==""), ]
new_ss<-new_ss[!(is.na(new_ss$Longitude) | new_ss$Longitude==""), ]
new_ss<-new_ss[!(is.na(new_ss$Outcome) | new_ss$Outcome==""), ]
new_ss$Outcome <- factor(new_ss$Outcome)
new_ss$Gender <- factor(new_ss$Gender)
new_ss$Object.of.search <- factor(new_ss$Object.of.search)
new_ss$Officer.defined.ethnicity <- factor(new_ss$Officer.defined.ethnicity)
new_ss$Age.range <- factor(new_ss$Age.range)


new_ss <- subset(new_ss, select = -c(Self.defined.ethnicity,ï..Type) )

#---------------------------------------------------------------
#Dividing Date into two seprate columns

new_ss$New_Date <- sapply(strsplit(as.character(new_ss$Date), "T"), "[", 1)
new_ss$Time <- sapply(strsplit(as.character(new_ss$Date), "T"), "[", 2)

new_ss <- mutate(new_ss,new_date = as.Date(new_ss$New_Date))
head(new_ss)



new_ss$year <- year(ymd(new_ss$new_date))
new_ss$month <- month(ymd(new_ss$new_date)) 
new_ss$year <- as.factor(new_ss$year)
new_ss$month <- as.factor(new_ss$month)

new_ss = subset(new_ss, select = -c(Date,Time,New_Date,new_date) )

str(new_ss)

summary(new_ss)


#------------------
#Renaming the column_to_rownames

names(new_ss)[names(new_ss) == "Object.of.search"] <- "Crime.type"
#_---------------------------------
#Modelling part start

str(new_ss)
describe(new_ss)
missmap(new_ss)
pairs(new_ss)
?set.seed
set.seed(223)
#----------------------



### predictors - Gender, Crime.type,AgeRange , Outcome,Ethnicity
names(training)

training_data <- subset(new_ss, select = -c(Gender,Age.range,Officer.defined.ethnicity) )


sub_gender <- subset(new_ss, select = -c(Age.range,Officer.defined.ethnicity) )
sub_Age <- subset(new_ss, select = -c(Gender,Officer.defined.ethnicity) )
sub_ethinicity <- subset(new_ss, select = -c(Age.range,Gender) )

names(sub_ethinicity)

prop.table(table(new_ss$Age.range)) * 100
prop.table(table(new_ss$Officer.defined.ethnicity)) * 100
prop.table(table(new_ss$Crime.type)) * 100
prop.table(table(new_ss$Outcome)) * 100
prop.table(table(new_ss$Gender)) * 100


# Making training and test

indxTrain_gender <- createDataPartition(y = sub_gender$Gender,p = 0.75,list = FALSE)
training_gender <- sub_gender[indxTrain_gender,]
testing_gender <- sub_gender[-indxTrain_gender,]

indxTrain_Age <- createDataPartition(y = sub_Age$Age.range,p = 0.75,list = FALSE)
training_Age <- sub_Age[indxTrain_Age,]
testing_Age <- sub_Age[-indxTrain_Age,]

indxTrain_Eth <- createDataPartition(y = sub_ethinicity$Officer.defined.ethnicity,p = 0.75,list = FALSE)
training_Eth <- sub_ethinicity[indxTrain_Eth,]
testing_Eth <- sub_ethinicity[-indxTrain_Eth,]

library(e1071)
library(randomForest)

# Modeliing
# using Random Forest, MLR and Naive Bayes

rf_gender = randomForest(Gender ~ ., data=training_gender,ntree=1000, importance=TRUE)
rf_gender

rf_gender = randomForest(Gender ~ ., data=training_gender,mtry = 3, importance=TRUE)
rf_gender

rf_gender = randomForest(Gender ~ ., data=training_gender, importance=TRUE)
rf_gender

#Gender Predictions
rf_Age = randomForest(Age.range ~ ., data=training_Age,ntree=2000, importance=TRUE)
rf_Age
names(training_Age)
rf_Age = randomForest(Age.range ~ ., data=training_Age,mtry = 3,ntree = 1000, importance=TRUE)
rf_Age
names(training_Age)

rf_Age = randomForest(Age.range ~ ., data=training_Age, importance=TRUE)
rf_Age
names(training_Age)

for(mtry in 1:6){
  rf_Age = randomForest(Age.range ~ ., data=training_Age,mtry = mtry, importance=TRUE)
  print(mtry)
  print(rf_Age$confusion)
}

rf_Eth = randomForest(Officer.defined.ethnicity ~ ., data=training_Eth,ntree=1000, importance=TRUE)
rf_Eth

rf_Eth = randomForest(Officer.defined.ethnicity ~ ., data=training_Eth,mtry = 4,ntree=1000, importance=TRUE)
rf_Eth
#
rf_Eth = randomForest(Officer.defined.ethnicity ~ ., data=training_Eth, importance=TRUE)
rf_Eth

# Naive Bayes Classifier
Naive_Bayes_Model_gender=naiveBayes(Age.range ~., data=training_Age)
#W printing the model summary
Naive_Bayes_Model_gender

#Prediction on the dataset 
NB_Predictions=predict(Naive_Bayes_Model_gender,training_Age)
# Confusion Matrix
table(NB_Predictions,training_Age$Age.range)



#--------------------------------------------------------------------------------

varImpPlot(rf_Age)


#------------------------------------------------------------------------------

prediction_for_table <- predict(rf_Age,testing_Age[,-3])
prediction_for_table2 <- predict(rf_classifier,testing[,-3])
#table(observed=testing[,5],predicted=prediction_for_table)
table(observed=training$Gender,predicted=prediction_for_table)
table(observed=testing$Gender,predicted=prediction_for_table2)



library(mlr) 

task = makeClassifTask(data = training, target = "Gender")
#Initialize the Naive Bayes classifier
selected_model = makeLearner("classif.naiveBayes")
#Train the model
NB_mlr = train(selected_model, task) 

NB_mlr$learner.model

head(training[3])
predictions_mlr = as.data.frame(predict(NB_mlr, newdata = training[-3]))
##Confusion matrix to check accuracy
table(predictions_mlr[,1],training$Gender)
table(NB_Predictions,training$Gender)
table(observed=training$Gender,predicted=prediction_for_table)
table(observed=testing$Gender,predicted=prediction_for_table2)

predictions_mlr

summary(training)




task_gender = makeClassifTask(data = training_gender, target = "Gender")
# Initialising the Naive Bayes classifier
selected_model_gender = makeLearner("classif.naiveBayes")
# Training the model
NB_mlr_gender = train(selected_model_gender, task_gender) 

NB_mlr_gender$learner.model

head(training[3]) 
predictions_mlr_gender = as.data.frame(predict(NB_mlr_gender, newdata = training[-3]))
##Confusion matrix to check accuracy
table(predictions_mlr_gender[,1],training$Gender)
table(NB_Predictions,training$Gender)
table(observed=training$Gender,predicted=prediction_for_table)
table(observed=testing$Gender,predicted=prediction_for_table2)

predictions_mlr











crime_new <- read.csv("C:/Users/ASUS/Downloads/final assignments/data science/full data set/Final Data/london_crime.csv",header = TRUE)
crime_new <- mutate(crime_new,new_date = as.Date(as.yearmon(crime$Month)))

crime_new$year <- year(ymd(crime_new$new_date))
crime_new$month <- month(ymd(crime_new$new_date)) 
crime_new$year <- as.factor(crime_new$year)
crime_new$month <- as.factor(crime_new$month)

crime_new = subset(crime_new, select = -c(Context,Reported.by,new_date) )
crime_new<-crime_new[!(is.na(crime_new$LSOA.code) | crime_new$LSOA.code==""), ]
crime_new<-crime_new[!(is.na(crime_new$Crime.type) | crime_new$Crime.type==""), ]
crime_new<-crime_new[!(is.na(crime_new$Last.outcome.category) | crime_new$Last.outcome.category==""), ]
crime_new<-crime_new[!(is.na(crime_new$Longitude) | crime_new$Longitude==""), ]
crime_new<-crime_new[!(is.na(crime_new$Latitude) | crime_new$Latitude==""), ]
crime_new<-crime_new[!(is.na(crime_new$Location) | crime_new$Location==""), ]
crime_new<-crime_new[!(is.na(crime_new$ï..Crime.ID) | crime_new$Longitude==""), ]
crime_new = subset(crime_new, select = -c(Falls.within,Location,ï..Crime.ID,Month) )


summary(crime_new)
levels(crime_new$Last.outcome.category)
levels(crime_new$LSOA.name)
#crime$Month <- month.abb[as.numeric(crime$Month)]

levels(crime_new$Last.outcome.category) <- c(levels(crime$Last.outcome.category), "Pending","Guilty","Not Guilty","Others")

crime_new$Last.outcome.category[crime_new$Last.outcome.category == "Action to be taken by another organisation"] <- "Pending"
crime_new$Last.outcome.category[crime_new$Last.outcome.category == "Awaiting court outcome"] <- "Pending"
crime_new$Last.outcome.category[crime_new$Last.outcome.category ==  "Court case unable to proceed"] <- "Pending"
crime_new$Last.outcome.category[crime_new$Last.outcome.category == "Court result unavailable"] <- "Others"
crime_new$Last.outcome.category[crime_new$Last.outcome.category == "Defendant found not guilty"] <- "Not Guilty"
crime_new$Last.outcome.category[crime_new$Last.outcome.category == "Defendant sent to Crown Court"] <- "Pending"
crime_new$Last.outcome.category[crime_new$Last.outcome.category == "Formal action is not in the public interest"] <- "Others"
crime_new$Last.outcome.category[crime_new$Last.outcome.category == "Further investigation is not in the public interest"] <- "Others"
crime_new$Last.outcome.category[crime_new$Last.outcome.category == "Investigation complete; no suspect identified"] <- "Not Guilty"
crime_new$Last.outcome.category[crime_new$Last.outcome.category == "Local resolution"] <- "Pending"
crime_new$Last.outcome.category[crime_new$Last.outcome.category == "Offender deprived of property"] <- "Guilty"
crime_new$Last.outcome.category[crime_new$Last.outcome.category == "Offender fined"] <- "Guilty"
crime_new$Last.outcome.category[crime_new$Last.outcome.category == "Offender given a caution"] <- "Guilty"
crime_new$Last.outcome.category[crime_new$Last.outcome.category == "Offender given a drugs possession warning"] <- "Guilty"
crime_new$Last.outcome.category[crime_new$Last.outcome.category == "Offender given community sentence"] <- "Guilty"
crime_new$Last.outcome.category[crime_new$Last.outcome.category == "Offender given conditional discharge"] <- "Guilty"
crime_new$Last.outcome.category[crime_new$Last.outcome.category == "Offender given penalty notice"] <- "Guilty"
crime_new$Last.outcome.category[crime_new$Last.outcome.category == "Offender given suspended prison sentence"] <- "Guilty"
crime_new$Last.outcome.category[crime_new$Last.outcome.category == "Offender otherwise dealt with"] <- "Guilty"
crime_new$Last.outcome.category[crime_new$Last.outcome.category == "Offender sent to prison"] <- "Guilty"
crime_new$Last.outcome.category[crime_new$Last.outcome.category == "Status update unavailable"] <- "Others"
crime_new$Last.outcome.category[crime_new$Last.outcome.category == "Unable to prosecute suspect"] <- "Others"
crime_new$Last.outcome.category[crime_new$Last.outcome.category == "Under investigation"] <- "Pending"

crime_new$Last.outcome.category <- factor(crime_new$Last.outcome.category)
summary(crime_new)


levels(crime_new$Crime.type)

levels(crime_new$Crime.type) <- c(levels(crime$Crime.type), "Drugs","Social Disorder","Theft","Criminal Activity", "Other Crime", "Weapons")
crime_new$Crime.type[crime_new$Crime.type ==  "Anti-social behaviour"] <- "Social Disorder"
crime_new$Crime.type[crime_new$Crime.type ==  "Burglary"] <- "Theft"
crime_new$Crime.type[crime_new$Crime.type ==  "Drugs"] <- "Drugs"
crime_new$Crime.type[crime_new$Crime.type ==  "Other theft"] <- "Theft"
crime_new$Crime.type[crime_new$Crime.type ==  "Public order"] <- "Social Disorder"
crime_new$Crime.type[crime_new$Crime.type ==  "Shoplifting"] <- "Theft"
crime_new$Crime.type[crime_new$Crime.type ==  "Vehicle crime"] <- "Theft"
crime_new$Crime.type[crime_new$Crime.type ==  "Bicycle theft"] <- "Theft"
crime_new$Crime.type[crime_new$Crime.type ==  "Criminal damage and arson"] <- "Criminal Activity"
crime_new$Crime.type[crime_new$Crime.type ==  "Other crime"] <- "Other Crime"
crime_new$Crime.type[crime_new$Crime.type ==  "Possession of weapons"] <- "Weapons"
crime_new$Crime.type[crime_new$Crime.type ==  "Robbery"] <- "Theft"
crime_new$Crime.type[crime_new$Crime.type ==  "Theft from the person"] <- "Theft"
crime_new$Crime.type[crime_new$Crime.type ==  "Violence and sexual offences"] <- "Criminal Activity"

crime_new$Crime.type <- factor(crime_new$Crime.type)

new_crime <- subset(crime_new, select = -c(LSOA.code,LSOA.name) )

names(new_crime)[names(new_crime) == "Last.outcome.category"] <- "Outcome"

new_crime$Age.RAnge <- predict(rf_Age,new_crime)
new_crime$Gender <- predict(rf_gender,new_crime)
new_crime$Officer.defined.Ethnicity  <- predict(rf_Eth,new_crime)




gender_count<- new_crime %>% group_by(Gender) %>% dplyr::summarise(count=n())%>% mutate(pct = count/sum(count))
gender_count

gender_count_percent_dist <- count_geom_bar_percent(gender_count,gender_count$Gender,gender_count$pct,"Gender","Percent","Number of Stop and Search(Gender Based) during 2017-20","Analysis of Stop and Search 2017-20")
gender_count_percent_dist



gender_count <-gender_count%>%
  mutate(lab.ypos = cumsum(pct) - 0.5*pct)
gender_count
ggplot(gender_count, aes(x="", y=pct, fill= factor(Gender)))+geom_bar(stat="identity")+
  coord_polar(theta="y")+
  theme(axis.line = element_blank(), panel.background = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  geom_text(aes(y=lab.ypos,label = scales::percent(pct,accuracy = .11) ),vjust =0,color ="black")+
  labs(x=NULL, y=NULL, fill="Gender",
       title="Gender Proportions - Stop and Search London",
       caption="Stop and Search- City of London Analysis")


ethnicity_count<- new_crime %>% group_by(Officer.defined.Ethnicity) %>% dplyr::summarise(count=n()) %>%mutate(pct = count/sum(count))

ethnicity_count_dist <- count_geom_bar(ethnicity_count,ethnicity_count$Officer.defined.Ethnicity,ethnicity_count$count,"Officer Defined Ethnicity","Count","Freq. Dist. of Officer Defined Ethnicity","Analysis of Stop and Search 2017-20")
ethnicity_count_dist

ethnicity_count_percent_dist <- count_geom_bar_percent(ethnicity_count,ethnicity_count$Officer.defined.Ethnicity,ethnicity_count$pct,"Officer Defined Ethnicity","Percent"," Percentage Dist. of Officer Defined Ethnicity","Analysis of Stop and Search 2017-20")
ethnicity_count_percent_dist

#Age based count
age_range_count<- new_crime %>% group_by(Age.RAnge) %>% dplyr::summarise(count=n())%>%mutate(pct = count/sum(count))

age_range_count_dist <- count_geom_bar(age_range_count,age_range_count$Age.RAnge,age_range_count$count,"Age Range","Count","Freq. Dist. of Age Range","Analysis of Stop and Search 2017-20")
age_range_count_dist

age_range_count_percent_dist <- count_geom_bar_percent(age_range_count,age_range_count$Age.RAnge,age_range_count$pct,"Age Range","Percent"," Percentage Dist. of Age Range","Analysis of Stop and Search 2017-20")
age_range_count_percent_dist

