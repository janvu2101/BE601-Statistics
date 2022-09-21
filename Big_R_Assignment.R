install.packages("tinytex")
tinytex::install_tinytex()
install.packages("kableExtra")
install.packages("readxl")
install.packages("ggplot2")
install.packages("tibble")
install.packages("RColorBrewer")

library(readxl) # For loading Excel files
library(kableExtra) # For table formatting
library(ggplot2) # For graphs
library(tinytex) # For making a pdf
library(tibble) # For the temporary data
library(dplyr)
library(tidyr)
library(RColorBrewer) # For heat map
options(scipen=999) # Fix y-axis formats (for R in general)

data <- read_excel("data/25701.xlsx",1)
data$date <- as.Date(data$date)
data[data<0] <- 0 # To simplify: We set negative values to zero

#PART A 
# Enter column to calculate
column <- 5

# Select dates
date1 <- "2020-03-31"
date2 <- "2021-03-31"
date3 <- "2022-03-31"

# Filter data
data1 <- data[data$date == date1, column]
data2 <- data[data$date == date2, column]
data3 <- data[data$date == date3, column]

# Create functions
functions<-function(x){
  list("N"            = sum (!is.na(x)),
       "Min"          = min (x, na.rm = TRUE),
       "Max"          = max (x, na.rm = TRUE),
       "1st Quartile" = format(round(as.vector(quantile(x, probs = 0.25, na.rm = TRUE)),2), nsmall=2),
       "Median"       = format(round(median (x, na.rm = TRUE),2),nsmall=2),         
       "3rd Quartile" = format(round(as.vector(quantile(x, probs = 0.75, na.rm = TRUE)),2), nsmall=2),
       "Mean"         = format(round(mean(x, na.rm = TRUE),2), nsmall=2),
       "Stdev"        = format(round(sd  (x, na.rm = TRUE),2),nsmall=2))
  }

# Apply function to data
column1<-sapply(data1, functions)
column2<-sapply(data2, functions)
column3<-sapply(data3, functions)

#Create table
table<-cbind(column1,column2,column3)

#Format table
colnames(table)<-c(date1,date2,date3)

# Print table
kable(table,
      format="latex",
      caption="Total COVID-19 Cases",
      align=rep('r',5),
      booktabs=TRUE)%>%
  kable_styling(latex_options = 
                  c("striped", "hold_position"))

print(table)

# Pick column to plot
data_column<-5

# Pick column to sort after
sort_column1<-5 
which(colnames(data)=="gdp_per_capita")
sort_column2<-15

# Pick column for x-axis names
name_column<-3

# Select end date
date<-"2022-03-31"

# Filter data for selected date
filtered_data<-data[data$date==date,]

# Sort data by s variable in decreasing order
sorted_data1<-filtered_data[order(-filtered_data[,sort_column1]),]
sorted_data2<-filtered_data[order(-filtered_data[,sort_column2]),]

# Transpose data (for chosen column)
transposed_data1<-t(sorted_data1[,data_column])
transposed_data2<-t(sorted_data2[,data_column])

# Create names
names1<-t(sorted_data1[,name_column])
names2<-t(sorted_data2[,name_column])
names1

# Specify plot margins (bottom, left, top, right)
margins<-par(mar=c(10,5,2,2))

# Create plots
barplot(transposed_data1, 
        main="Total COVID-19 cases per country",
        names.arg = names1,
        las=2)

barplot(transposed_data2, 
        main="Total COVID-19 cases per country (sorting: GDP/Cap)",
        names.arg = names2,
        las=2)

# Remove margins from workspace
rm(margins)

# Pick column
column<-5

# Select dates
date1<-"2021-03-31"
date2<-"2022-03-31"

# Filter data
box_data<-data[data$date==date1 | data$date==date2,]

# Cases per GDP/Capita
box_data$cases_per_GDP <- (box_data$total_cases/box_data$gdp_per_capita)

# Cases per popolation density
box_data$cases_per_pop_den <- (box_data$total_cases/box_data$population_density)

# Apply margins to plots
margins<-par(mar=c(5,10,2,2))

boxplot(box_data$total_cases_per_million~box_data$date,
        horizontal=TRUE,
        las=1,
        main="Total COVID-19 Cases per Million",
        xlab="",
        ylab="")
boxplot(box_data$total_cases_per_million~box_data$date,
        horizontal=TRUE,
        log = "x",
        las=1,
        main="Total COVID-19 Cases per Million (log scale)",
        xlab="",
        ylab="")
rm(margins)

boxplot(box_data$cases_per_GDP~box_data$date,
        horizontal=TRUE,
        log = "x",
        las=1,
        main="Total COVID-19 Cases per GDP (log scale)",
        xlab="",
        ylab="")

boxplot(box_data$cases_per_pop_den~box_data$date,
        horizontal=TRUE,
        log = "x",
        las=1,
        main="Total COVID-19 Cases per population density (log scale)",
        xlab="",
        ylab="")

# Select dates
date1<-"2020-04-01"
date2<-"2021-03-31"
date3<-"2021-04-01"
date4<-"2022-03-31"

# Filter data
data1<-data[data$date>=date1&data$date<=date2,]
data2<-data[data$date>=date3&data$date<=date4,]

a1<-aggregate(data1$new_cases, by=list(data1$location), FUN=sum)
a2<-aggregate(data2$new_cases, by=list(data2$location), FUN=sum)
a <-na.omit(cbind(a1,y=a2$x))

# Apply margins to plots
margins<-par(mar=c(5,10,2,2))

# Draw plot
plot(a$x,a$y,
     las=0,
     main="Total COVID-19 Cases",
     xlab=paste(date1," to ",date2,sep=""),
     ylab=paste(date3," to ",date4,sep=""))

# Draw log-plot
plot(a$x,a$y,
     las=0,
     log = c("x","y"),
     main="Total COVID-19 Cases (log scale)",
     xlab=paste(date1," to ",date2,sep=""),
     ylab=paste(date3," to ",date4,sep=""))

# Remove margins from workspace
rm(margins)

# PART B
unique(data$location)

# Select Countries
Country1<-"Sweden"
Country2<-"Peru"
Country3<-"El Salvador"
Country4<-"Russia"
Country5<-"Cambodia"


cat(Country1)
cat(Country2)
cat(Country3)
cat(Country4)
cat(Country5)

# B1 - Descriptive Statistics

# Pick the column of the variable you want to examine
colnames(data)
column<-which(colnames(data) == "new_cases")

# Filter data
data1<-data[data$location==Country1, column]
data2<-data[data$location==Country2, column]
data3<-data[data$location==Country3, column]
data4<-data[data$location==Country4, column]
data5<-data[data$location==Country5, column]

# Create functions
functions<-function(x){
  list("N"            = sum (!is.na(x)),
       "Min"          = min (x, na.rm = TRUE),
       "Max"          = max (x, na.rm = TRUE),
       "1st Quartile" = format(round(as.vector(quantile(x, probs = 0.25, na.rm = TRUE)),2),nsmall=2),
       "Median"       = format(round(median (x, na.rm = TRUE),2),nsmall=2),         
       "3rd Quartile" = format(round(as.vector(quantile(x, probs = 0.75, na.rm = TRUE)),2),nsmall=2),
       "Mean"         = format(round(mean(x, na.rm = TRUE),2),nsmall=2),
       "Stdev"        = format(round(sd  (x, na.rm = TRUE),2),nsmall=2))
  }

# Apply functions to data
column1<-sapply(data1, functions)
column2<-sapply(data2, functions)
column3<-sapply(data3, functions)
column4<-sapply(data4, functions)
column5<-sapply(data5, functions)

# Create table
table<-cbind(column1,column2,column3,column4,column5)

# Format table
colnames(table)<-c(Country1,Country2,Country3,Country4,Country5)

# Print table
kable(table,
      format="latex",
      caption="Daily New COVID-19 Cases",
      align=rep('r',5),
      booktabs=TRUE)%>%
  kable_styling(latex_options = 
                  c("striped", "hold_position"))
print(table)

#B2 - Histograms

# Filter data
data1<-data[data$location==Country1,]
data2<-data[data$location==Country2,]
data3<-data[data$location==Country3,]
data4<-data[data$location==Country4,]
data5<-data[data$location==Country5,]

# Specify plot margins
margins<-par(mar=c(10,5,2,2))

barplot(height=data1$new_cases, 
        main=Country1,
        names= data1$date,
        las=2)

barplot(height=data2$new_cases, 
        main=Country2,
        names= data2$date,
        las=2)

barplot(height=data3$new_cases, 
        main=Country3,
        names= data3$date,
        las=2)

barplot(height=data4$new_cases, 
        main=Country4,
        names= data4$date,
        las=2)

barplot(height=data5$new_cases, 
        main=Country5,
        names= data5$date,
        las=2)

# Remove margins from workspace
rm(margins)

# B3 - Box plots

# Filter data
data<-data[data$location==Country1|data$location==Country2|data$location==Country3|data$location==Country4|data$location==Country5,]

# Plot margins
m <- par(mar=c(5,10,2,2))

#Make a boxplot of daily cases
boxplot(data$new_cases~data$location,
        horizontal = TRUE,
        las=1,
        main="Daily COVID 19 Cases",
        xlab="",
        ylab="")

p <- data[,6]
q <- data.frame("new_cases2"= rep(1/10,nrow(p)))
t <- data.frame("A" = p, "B" = q)
names(t)[1] <- "A"
names(t)[2] <- "B"
t$new_cases <- apply(t, 1, max)
data[,6] <- t[,3]

boxplot(data$new_cases~data$location,
        horizontal = TRUE,
        log="x",
        las=1,
        main="Daily COVID 19 Cases",
        xlab="",
        ylab="")

# Apply margins
rm(m)

# B4 - Times Series plots

# We add a variable to our data
data["people_fully_vaccinated_per_100"] <- round(100*data[,18]/data[,13],2)
colnames(data)

# Specify plot margins
m<-par(mar=c(10,5,2,2))

#Plot new cases over time 
data %>%
  ggplot(aes(x=date, y=new_cases, group=location, color=location)) +
  geom_line(na.rm=TRUE) +
  ggtitle("New Cases") + 
  xlab("Date") + ylab("Cases") +
  labs(colour = "Country")

#Plot total cases per million over time
data %>%
  ggplot(aes(x=date, y=total_cases_per_million, group=location, color=location)) +
  geom_line(na.rm=TRUE) + 
  ggtitle("Total Cases per million Capita") + 
  xlab("Date") + ylab("Total Cases / mCap") +
  labs(colour = "Country")

#Plot total deaths per million over time 
data %>%
  ggplot(aes(x=date, y=total_deaths_per_million, group=location, color=location)) +
  geom_line(na.rm=TRUE) +
  ggtitle("Total Deaths per million Capita") + 
  xlab("Date") + ylab("Total Deaths / mCap") +
  labs(colour = "Country")

#Plot total vaccinations per 100 over time 
data %>%
  ggplot( aes(x=date, y=people_fully_vaccinated_per_100, group=location, color=location)) +
  geom_line(na.rm=TRUE) +
  ggtitle("Fully Vaccinated per hundred Capita in 2021") + 
  xlab("Date") + ylab("Fully Vaccinated / 100 people") +
  labs(colour = "Country") +
  scale_x_date(limit=c(as.Date("2021-01-01"),as.Date("2022-08-30")))

# Remove margins from workspace
rm(m)

# B5 - Pie Charts

#Pick a date
date1<-"2022-08-15"

datax<-data[data$date==date1,]
lbls <- datax$location
cases <- datax$total_cases
deaths <- datax$total_deaths
vaccs <- datax$total_vaccinations
pctc <- round(cases/sum(cases)*100)
pctd <- round(deaths/sum(deaths)*100)
pctv <- round(vaccs/sum(vaccs)*100)
lblsc <- paste(lbls, pctc)
lblsd <- paste(lbls, pctd)
lblsv <- paste(lbls, pctv)
lblsc <- paste(lblsc,"%",sep="")
lblsd <- paste(lblsd,"%",sep="")
lblsv <- paste(lblsv,"%",sep="")
pie(cases, labels = lblsc, main="Total Cases")

# PART C

#C1 - Table
data<-data[data$location==Country1|data$location==Country2|data$location==Country3|data$location==Country4|data$location==Country5,]
data$date1<-as.numeric(format(as.Date(data$date), "%m"))
data$date2<-as.numeric(format(as.Date(data$date), "%Y"))
data$date3<-data$date2+data$date1*0.01

country<-data$location
month<-format(data$date3,digits=2, nsmall=2)
cases<-round(data$new_cases_per_million, 0)

df<-data.frame(country,month,cases)

df<-df %>%
  group_by(country,month) %>%
  summarise(cases=sum(cases))

df<-spread(df,month,cases)

df<-with(df, df[order(df$country, decreasing = TRUE),])

kable(df,
      format="latex",
      caption="New Cases per million per Month",
      align=rep('r',5),
      booktabs=TRUE) %>%
  kable_styling(latex_options =
                  c("striped", "hold_position", "scale_down"))

#C2 - Heat Map
Month <- rep(c(1:32), times = 5)
Country <- rep(c("Sweden","Peru","El Salvador","Russia","Cambodia"), each=32)

Sweden <- c(NA,1,460,1603,1630,2794,876,699,813,3010,11346,18558,12380,8633,14099,16119,9064,2055,960,2559,2490,1779,3186,10502,72197,35414,4462,1249,672,1075,1960,NA)
Russia <- c(0,0,12,717,2063,1662,1321,1062,1230,2999,4618,5867,4692,2685,2039,1766,1810,3005,5070,4381,4004,6729,7514,5870,9304,30953,9797,2304,1012,671,1169,5465)
Peru <- c(NA,NA,30,1065,3783,3582,3628,7108,4880,2622,1847,1560,3651,5506,6672,7433,4629,2863,1759,1133,762,785,1026,1795,27961,8238,888,538,NA,1360,8397,5635)
El_Salvador <- c(NA,NA,5,58,336,620,1615,1440,NA,751,810,1159,1382,826,718,711,714,802,1243,1179,1629,1437,1011,340,2084,3367,742,164,105,1091,3353,0)
Cambodia <- c(0,0,5,0,0,1,5,1,0,0,0,2,3,21,96,663,1008,1222,1619,952,1182,355,95,18,53,537,321,39,0,1,28,45)

Cases <- c(Sweden,Russia,Peru,El_Salvador, Cambodia)
df <- data.frame(Month, Country, Cases)

df %>% ggplot(aes(Month, Country, fill = Cases)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") +
  theme_minimal() +  
  theme(panel.grid = element_blank(), 
        legend.position="bottom", 
        text = element_text(size = 8)) +
  ggtitle("COVID-19") + 
  ylab("") + xlab("")

# C3 - Mean and Std









