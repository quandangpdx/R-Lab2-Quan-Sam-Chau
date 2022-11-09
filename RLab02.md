---
title: "R Coding Lab Part 2"
output: rmdformats::html_docco
---



**Complete the following lab as a group. This document should exist in your GitHub repo while you're working on it. Your code should be heavily commented so someone reading your code can follow along easily. See the first code snippet below for an example of commented code.**

**Here's the catch: For any given problem, the person writing the code should not be the person commenting that code, and every person must both code AND comment at least one problem in this lab (you decide how to split the work). This will involve lots of pushing and pulling through Git, and you may have to resolve conflicts if you're not careful! Refer to last Thursday's class notes for details on conflict resolution.**


# Playing With Cherry Blossom Race Data

1) First load the data, which is saved as a .RData file called `CBdata.1_10.RData`. This is the first ten years' worth of Cherry Blossom Race data. Pull out the data associated with 1976 and store it as a data frame called `dat.76`. Remove the column `Pis/Tis` using a `dplyr` function.



```r
# Loading the cherry blossom data
library(dplyr)
load("CBdata.1_10.RData")

# Now write code to remove the specified column
dat.76 <- data.frame(CBdata.1_10[4])
dat.76 <- dat.76 %>% select(-PiS.TiS)
```


2) Use the `summarise()` function to find the mean and median recorded ages in 1976.


```r
# find and display the mean and median ages from 1976
dat.76 %>% summarise(mean_age=mean(Age, na.rm=T),
                     median_age=median(Age, na.rm = T))
```

```
##   mean_age median_age
## 1 32.09356         32
```


3) You might have noticed that a number of age values are missing (i.e. `NA`). Your next goal is to use `dplyr` to remove the data with missing age. This should not be a loop!



```r
# remove participants with unrecorded age
dat.76.clean <- dat.76 %>% filter(!is.na(Age))
```


4) Last week you wrote a loop to combine all of the data from `CBdata.1_10` into one cleaned data frame. Use the function `bind_rows()` from `dplyr` to accomplish this same task. use the `?` command to look up documentation on functions you're not familar with like this: `?bind_rows`. Make sure your final data frame has neither the `Pis/Tis` column nor `NA` Age values.
Use the `identical()` function to verify that the 1976 data in this larger cleaned data set is the same as the cleaned version of `dat.76`.


```r
# comnine all data into a single data frame
allData <- bind_rows(CBdata.1_10)

# remove Pis/Tis column and observations with unrecorded age
allData <- allData %>% filter(!is.na(Age)) %>% select(-`PiS/TiS`)

# compare results to cleaned data from last week's lab
# (supposed to be identical - 481 observations of 9 variables?)
identical(dat.76.clean, allData %>% filter(Year==1976))
```

```
## [1] FALSE
```


5) Now that you have the combined data set for these 10 years, let's compare some basic results to what you found last week. Use piping and `dplyr` functions in your computations.
a) Calculate the average of the recorded ages in 1976 and that same average over the entire `CBdata.1_10` data set, and make sure these numbers match the ones you found in Lab 1.


```r
# compute and display average age of participants in 1976
dat.76.clean %>% summarise(mean_76=mean(Age))
```

```
##    mean_76
## 1 32.09356
```

```r
# compare with results from last week's lab
allData %>% summarise(mean_all=mean(Age))
```

```
##   mean_all
## 1 33.26157
```

b) Recall that the `CBdata.1_10` contains the first ten year's worth of cherry blossom race data. Compute the average participant age over the first five years and the average age over years 6-10, and make sure these numbers match the ones you found in Lab 1.


```r
# compare average age of participants in years before 1978 to
# that in years in 1978 and after
allData %>% filter(Year<1978) %>% summarise(mean_all=mean(Age))
```

```
##   mean_all
## 1 31.11927
```

```r
allData %>% filter(Year>=1978) %>% summarise(mean_all=mean(Age))
```

```
##   mean_all
## 1 33.69101
```

6) Let's now incorporate weather data into our cherry blossom data set. We will be dealing with multiple data sources, so this is a perfect oppritunity to practice `join` skills...
a) use `readr()` to import the `weatherdat.csv` data. This is raw data recorded by a weather station in the Washington DC area. This particular data set contains daily summaries of various weather measurements.


```r
library(readr)

# import weather data
weather <- read.csv("weatherdat.csv")
```

b) Open the `Rite_of_Spring_1973_2020-1.pdf` document, and record the dates of the first 10 races. Store this information in a vector or data frame.


```r
library(pdftools)
library(stringr)
library(tidyr)

# import document with data on races
pdf <- pdf_text("Rite_of_Spring_1973_2020-1.pdf")

# define date pattern
regex_pattern <- "\\d{4}\\s\\((April|March)\\s\\d{1,2}\\)" # eg: 2020 (April 5)

# search for dates of races and save to a dataframe
raw_date <- unlist(str_match_all(pdf, regex_pattern))
raw_date <- as.data.frame.AsIs(raw_date)
View(raw_date)

# look for dates in April and March
raw_date <- raw_date %>% filter(raw_date!="April",raw_date!="March") %>% distinct() %>% separate(col=raw_date, into= c("year","month","day"),sep=c(" "))

# remove excess character from day string
raw_date$day <- str_sub(raw_date$day, 1, nchar(raw_date$day)-1)

# convert month name to number string
date <-  raw_date %>% mutate(
  m=case_when(month=="(April"~"04",
              month=="(March"~"03"))

# magic to convert dates to numeric values
date$day <- str_replace(date$day,nchar(date$day)==1,paste0("0",date$day[nchar(date$day)==1]))
```

```
## Error in UseMethod("type"): no applicable method for 'type' applied to an object of class "logical"
```

```r
date$day[nchar(date$day)==1] <- paste0("0",date$day[nchar(date$day)==1])
date$DATE <- paste(date$year,date$m,date$day, sep = "-")
date$year <- as.numeric(date$year)
```

c) Use a `join` function to add a date column to your cherry blossom data frame. Hints: (i) the `paste()` and `paste0` functions are useful for creating charecter vectors (ii) it would be useful for these dates to have the same format as those found in the weather data set...

```r
# add date column to cherry blossom data
allData <- allData %>% left_join(date %>% select(year,DATE), by = c("Year"="year"))
```

d) Use a `join` function to add precipitation `PRCP`  and minimum daily temperature `TMIN` columns to your cherry blossom data set.

```r
# add precipitation and temperature data to cherry blossom data
allData <- allData %>% left_join(weather %>% select(PRCP,TMIN,DATE), by = "DATE")
```


# Playing with the indoor positioning system data

The `IPS_sampledata` data set contains a fraction of the indoor positioning system data for 15 randomly sampled locations.This data set is loaded into memory using the chunk of R code below, complete the following exercises.


```r
# loads data set IPS_sampledata
load("IPS_portion.RData")
```

### Variable dictionary

- `time`: timestamp in milliseconds since midnight 01/01/1970 UTC

- `scanMac`: MAC address of the scanning device (this is a handheld device)

- `posX`, `posY` and `posZ`: the (x, y, z) physical coordinate of the scanning device

- `orientation`: degree orientation of the user carrying the scanning device in degrees

- `mac`: MAC address of an access point

- `signal`: signal strength in dBm (Decibel-milliwatts)

- `channel`: the channel frequency

- `type`: type of device (access point = 3, device in adhoc mode = 1)

### Let's clean up the data a bit

1. Apply the same `class` conversions you did last week to get these variables into the correct class type. Use `dplyr` functions and piping to complete this operation (there are many ways to do so). If you'd like to `mutate` multiple columns at once, the `across()` function might be useful!


```r
# using mutate to change the type of the data
IPS_sampledata <- IPS_sampledata %>%
    # data that was represented as character are now numeric
    mutate(
        time = as.numeric(time),
        posX = as.numeric(posX),
        posY = as.numeric(posY),
        posZ = as.numeric(posZ),
        orientation = as.numeric(orientation),
        signal = as.numeric(signal),
        channel = as.numeric(channel),
        type = as.numeric(type)
    );

# printing the summary of the sample data
summary(IPS_sampledata);
```

```
##       time            scanMac               posX            posY       
##  Min.   :1.14e+12   Length:106744      Min.   : 1.00   Min.   : 0.000  
##  1st Qu.:1.14e+12   Class :character   1st Qu.: 2.00   1st Qu.: 3.000  
##  Median :1.14e+12   Mode  :character   Median :11.00   Median : 4.000  
##  Mean   :1.14e+12                      Mean   :12.91   Mean   : 5.523  
##  3rd Qu.:1.14e+12                      3rd Qu.:22.00   3rd Qu.: 8.000  
##  Max.   :1.14e+12                      Max.   :32.00   Max.   :13.000  
##       posZ    orientation        mac                signal      
##  Min.   :0   Min.   :  0.2   Length:106744      Min.   :-96.00  
##  1st Qu.:0   1st Qu.: 90.1   Class :character   1st Qu.:-74.00  
##  Median :0   Median :180.1   Mode  :character   Median :-64.00  
##  Mean   :0   Mean   :168.3                      Mean   :-64.43  
##  3rd Qu.:0   3rd Qu.:270.3                      3rd Qu.:-54.00  
##  Max.   :0   Max.   :359.9                      Max.   :-26.00  
##     channel               type      
##  Min.   :2.412e+09   Min.   :1.000  
##  1st Qu.:2.427e+09   1st Qu.:3.000  
##  Median :2.437e+09   Median :3.000  
##  Mean   :2.438e+09   Mean   :2.664  
##  3rd Qu.:2.457e+09   3rd Qu.:3.000  
##  Max.   :2.472e+09   Max.   :3.000
```

2. Because we only want data relative to access points, remove observations that correspond to any other type of device using `dplyr` functions.


```r
# using the filter function to remove other observations
IPS_sampledata <- IPS_sampledata %>%
    filter(type == 3);

summary(IPS_sampledata);
```

```
##       time            scanMac               posX           posY       
##  Min.   :1.14e+12   Length:88831       Min.   : 1.0   Min.   : 0.000  
##  1st Qu.:1.14e+12   Class :character   1st Qu.: 2.0   1st Qu.: 3.000  
##  Median :1.14e+12   Mode  :character   Median :11.0   Median : 4.000  
##  Mean   :1.14e+12                      Mean   :12.7   Mean   : 5.555  
##  3rd Qu.:1.14e+12                      3rd Qu.:19.0   3rd Qu.: 8.000  
##  Max.   :1.14e+12                      Max.   :32.0   Max.   :13.000  
##       posZ    orientation        mac                signal      
##  Min.   :0   Min.   :  0.2   Length:88831       Min.   :-96.00  
##  1st Qu.:0   1st Qu.: 90.1   Class :character   1st Qu.:-70.00  
##  Median :0   Median :180.1   Mode  :character   Median :-61.00  
##  Mean   :0   Mean   :168.2                      Mean   :-61.93  
##  3rd Qu.:0   3rd Qu.:270.3                      3rd Qu.:-53.00  
##  Max.   :0   Max.   :359.9                      Max.   :-26.00  
##     channel               type  
##  Min.   :2.412e+09   Min.   :3  
##  1st Qu.:2.422e+09   1st Qu.:3  
##  Median :2.432e+09   Median :3  
##  Mean   :2.435e+09   Mean   :3  
##  3rd Qu.:2.442e+09   3rd Qu.:3  
##  Max.   :2.472e+09   Max.   :3
```

3. Last week you identified variables that provide redundant or no information. Remove them from the data frame using `dplyr` functions.


```r
# removing irrelevant columns with the select function
IPS_sampledata <- IPS_sampledata %>%
    select(-type, -scanMac, -channel, -posZ);

summary(IPS_sampledata);
```

```
##       time               posX           posY         orientation   
##  Min.   :1.14e+12   Min.   : 1.0   Min.   : 0.000   Min.   :  0.2  
##  1st Qu.:1.14e+12   1st Qu.: 2.0   1st Qu.: 3.000   1st Qu.: 90.1  
##  Median :1.14e+12   Median :11.0   Median : 4.000   Median :180.1  
##  Mean   :1.14e+12   Mean   :12.7   Mean   : 5.555   Mean   :168.2  
##  3rd Qu.:1.14e+12   3rd Qu.:19.0   3rd Qu.: 8.000   3rd Qu.:270.3  
##  Max.   :1.14e+12   Max.   :32.0   Max.   :13.000   Max.   :359.9  
##      mac                signal      
##  Length:88831       Min.   :-96.00  
##  Class :character   1st Qu.:-70.00  
##  Mode  :character   Median :-61.00  
##                     Mean   :-61.93  
##                     3rd Qu.:-53.00  
##                     Max.   :-26.00
```

4. Note that the `time` variable is in milliseconds.  Use `dplyr` to transform it into seconds and then convert its class into a time format using the function `as.POSIXct`.


```r
# changing the time column from numeric to date/time format
IPS_sampledata <- IPS_sampledata %>%
    mutate(time = as.POSIXct(time/1000, origin = "1970-01-01"));

summary(IPS_sampledata);
```

```
##       time                             posX           posY       
##  Min.   :2006-02-10 23:55:38.44   Min.   : 1.0   Min.   : 0.000  
##  1st Qu.:2006-02-11 05:40:50.76   1st Qu.: 2.0   1st Qu.: 3.000  
##  Median :2006-02-11 08:10:35.29   Median :11.0   Median : 4.000  
##  Mean   :2006-02-13 18:16:22.80   Mean   :12.7   Mean   : 5.555  
##  3rd Qu.:2006-02-19 01:31:22.59   3rd Qu.:19.0   3rd Qu.: 8.000  
##  Max.   :2006-02-19 11:50:45.80   Max.   :32.0   Max.   :13.000  
##   orientation        mac                signal      
##  Min.   :  0.2   Length:88831       Min.   :-96.00  
##  1st Qu.: 90.1   Class :character   1st Qu.:-70.00  
##  Median :180.1   Mode  :character   Median :-61.00  
##  Mean   :168.2                      Mean   :-61.93  
##  3rd Qu.:270.3                      3rd Qu.:-53.00  
##  Max.   :359.9                      Max.   :-26.00
```

5. Convert this data set to a more wide format by creating one column for each access point, with each of those columns containing the corresponding signal strengths. Hint: you should end up with a data frame that has a lot fewer rows!
Set this data set aside and use the long format data for the rest of the assignment


```r
# using pivot_wider to generate a wide format data frame

#   (!) this doesn't quite work, and generates a lot of NA's
#   (!) not sure how to fix it or why it happens :(

  # wide_IPS_sampledata <- IPS_sampledata %>%
  #   group_by(time, posX, posY, orientation, mac);

wide_IPS_sampledata <- IPS_sampledata %>%
    mutate(row = row_number()) %>%
    group_by(time, posX, posY, orientation, mac) %>%
    pivot_wider(names_from = mac, values_from = signal) %>%
    select(-row);

summary(wide_IPS_sampledata);
```

```
##       time                             posX           posY       
##  Min.   :2006-02-10 23:55:38.44   Min.   : 1.0   Min.   : 0.000  
##  1st Qu.:2006-02-11 05:40:50.76   1st Qu.: 2.0   1st Qu.: 3.000  
##  Median :2006-02-11 08:10:35.29   Median :11.0   Median : 4.000  
##  Mean   :2006-02-13 18:16:22.80   Mean   :12.7   Mean   : 5.555  
##  3rd Qu.:2006-02-19 01:31:22.59   3rd Qu.:19.0   3rd Qu.: 8.000  
##  Max.   :2006-02-19 11:50:45.80   Max.   :32.0   Max.   :13.000  
##                                                                  
##   orientation    00:14:bf:b1:97:8a 00:0f:a3:39:e1:c0 00:14:bf:b1:97:81
##  Min.   :  0.2   Min.   :-90.00    Min.   :-91.00    Min.   :-82.00   
##  1st Qu.: 90.1   1st Qu.:-63.00    1st Qu.:-58.00    1st Qu.:-64.00   
##  Median :180.1   Median :-57.00    Median :-54.00    Median :-55.00   
##  Mean   :168.2   Mean   :-55.83    Mean   :-54.19    Mean   :-56.33   
##  3rd Qu.:270.3   3rd Qu.:-50.00    3rd Qu.:-50.00    3rd Qu.:-49.00   
##  Max.   :359.9   Max.   :-26.00    Max.   :-34.00    Max.   :-34.00   
##                  NA's   :76814     NA's   :75647     NA's   :77869    
##  00:14:bf:b1:97:8d 00:0f:a3:39:dd:cd 00:0f:a3:39:e2:10 00:14:bf:b1:97:90
##  Min.   :-82.00    Min.   :-91.00    Min.   :-96.00    Min.   :-89.00   
##  1st Qu.:-63.00    1st Qu.:-78.00    1st Qu.:-93.00    1st Qu.:-74.00   
##  Median :-57.00    Median :-72.00    Median :-90.00    Median :-68.00   
##  Mean   :-56.45    Mean   :-71.26    Mean   :-89.77    Mean   :-65.37   
##  3rd Qu.:-51.00    3rd Qu.:-65.00    3rd Qu.:-88.00    3rd Qu.:-57.00   
##  Max.   :-33.00    Max.   :-51.00    Max.   :-81.00    Max.   :-34.00   
##  NA's   :77975     NA's   :75666     NA's   :87630     NA's   :77749    
##  00:14:bf:3b:c7:c6 00:0f:a3:39:e0:4b 00:04:0e:5c:23:fc 00:30:bd:f8:7f:c5
##  Min.   :-88.00    Min.   :-95.00    Min.   :-94.00    Min.   :-92.00   
##  1st Qu.:-67.00    1st Qu.:-90.00    1st Qu.:-93.00    1st Qu.:-90.00   
##  Median :-62.00    Median :-89.00    Median :-92.00    Median :-90.00   
##  Mean   :-60.51    Mean   :-87.73    Mean   :-92.45    Mean   :-89.66   
##  3rd Qu.:-54.00    3rd Qu.:-86.00    3rd Qu.:-92.00    3rd Qu.:-89.00   
##  Max.   :-40.00    Max.   :-76.00    Max.   :-91.00    Max.   :-87.00   
##  NA's   :77043     NA's   :84417     NA's   :88782     NA's   :88718
```


### Examining the data more closely

1. Using grouping and `dplyr` functions, tally the  number of observations for all access points in the data.

```r
# count number of observations from each access point
tally = IPS_sampledata %>%
  group_by(mac) %>%
  count()

summary(tally)
```

```
##      mac                  n        
##  Length:11          Min.   :   49  
##  Class :character   1st Qu.: 2808  
##  Mode  :character   Median :10962  
##                     Mean   : 8076  
##                     3rd Qu.:11902  
##                     Max.   :13184
```

2. While the researchers did their best to clean their data, some noise was introduced by access points on other floors.  Based on the number of counts, identify and remove likely suspects for access points read by mistake, again using `dplyr` functions.

```r
# look for distinct entries
cleaned = IPS_sampledata %>%
  distinct(time, posX, posY, orientation, mac, signal)

# note the difference in number of rows between the cleaned data and the original data
summary(cleaned)
```

```
##       time                             posX            posY       
##  Min.   :2006-02-10 23:55:38.44   Min.   : 1.00   Min.   : 0.000  
##  1st Qu.:2006-02-11 05:40:52.77   1st Qu.: 2.00   1st Qu.: 3.000  
##  Median :2006-02-11 08:10:37.56   Median :11.00   Median : 4.000  
##  Mean   :2006-02-13 18:21:15.82   Mean   :12.72   Mean   : 5.532  
##  3rd Qu.:2006-02-19 01:31:22.48   3rd Qu.:19.00   3rd Qu.: 8.000  
##  Max.   :2006-02-19 11:50:45.80   Max.   :32.00   Max.   :13.000  
##   orientation        mac                signal      
##  Min.   :  0.2   Length:87569       Min.   :-96.00  
##  1st Qu.: 90.1   Class :character   1st Qu.:-70.00  
##  Median :180.1   Mode  :character   Median :-61.00  
##  Mean   :168.1                      Mean   :-61.85  
##  3rd Qu.:270.3                      3rd Qu.:-53.00  
##  Max.   :359.9                      Max.   :-26.00
```

```r
summary(IPS_sampledata)
```

```
##       time                             posX           posY       
##  Min.   :2006-02-10 23:55:38.44   Min.   : 1.0   Min.   : 0.000  
##  1st Qu.:2006-02-11 05:40:50.76   1st Qu.: 2.0   1st Qu.: 3.000  
##  Median :2006-02-11 08:10:35.29   Median :11.0   Median : 4.000  
##  Mean   :2006-02-13 18:16:22.80   Mean   :12.7   Mean   : 5.555  
##  3rd Qu.:2006-02-19 01:31:22.59   3rd Qu.:19.0   3rd Qu.: 8.000  
##  Max.   :2006-02-19 11:50:45.80   Max.   :32.0   Max.   :13.000  
##   orientation        mac                signal      
##  Min.   :  0.2   Length:88831       Min.   :-96.00  
##  1st Qu.: 90.1   Class :character   1st Qu.:-70.00  
##  Median :180.1   Mode  :character   Median :-61.00  
##  Mean   :168.2                      Mean   :-61.93  
##  3rd Qu.:270.3                      3rd Qu.:-53.00  
##  Max.   :359.9                      Max.   :-26.00
```

3.  The orientation of the hand-held device considered was supposed to be exactly set to the 8 angles from 0-315 in increments of 45 degrees (360 is equivalent to 0). However, in practice the measured orientations were close to the 8 expected but had some error.  Use the `case_when` function to recode the orientation values as one of 0, 45, 90, 135, 180, 225, 270, 315. Call the recoded orientation variable `rec_orient`.

```r
# round each orientation to the nearest multiple of 45 degrees (modulo 360)
rec_orient = cleaned %>%
  select(orientation) %>%
  mutate(orientation =
    case_when(
      orientation %% 360 < 9 ~ 0,
      orientation %% 315 < 9 ~ 315,
      orientation %% 270 < 9 ~ 270,
      orientation %% 225 < 9 ~ 225,
      orientation %% 180 < 9 ~ 180,
      orientation %% 135 < 9 ~ 135,
      orientation %% 90 < 9 ~ 80,
      orientation %% 45 < 9 ~ 45,
      360 %% orientation < 9 ~ 0,
      315 %% orientation < 9 ~ 315,
      270 %% orientation < 9 ~ 270,
      225 %% orientation < 9 ~ 225,
      180 %% orientation < 9 ~ 180,
      135 %% orientation < 9 ~ 135,
      90 %% orientation < 9 ~ 80,
      45 %% orientation < 9 ~ 45,
      TRUE ~ as.numeric(orientation)
    )
  )

# count number of observations
rec_orient %>%
  group_by(orientation) %>%
  count()
```

```
## # A tibble: 8 × 2
## # Groups:   orientation [8]
##   orientation     n
##         <dbl> <int>
## 1           0 14360
## 2          45  8828
## 3          80  9258
## 4         135  7229
## 5         180 10874
## 6         225 10937
## 7         270 14833
## 8         315 11250
```

4. Last week you created the function `signal_summary` that takes as inputs a location (`posX`, `posY`, `posZ`), an orientation (`rec_orient`) and an access point id (`mac`).  The function identified and subset the rows in `IPS_sampledata` corresponding to this unique combination, then calculated and returned the mean and standard deviation for the corresponding signal strengths. You then used `lapply` to compute mean and standard deviation values for all combinations of location, orientation, and access point ID.
Use piping,`summarise()`, and other `dplyr` functions to run this same computation without the use of loops or `lapply`. Compare your results with those from last week to confirm you're doing the right thing!

```r
# compute average signal strength
cleaned %>%
  summarise(sigal_mean = mean(signal))
```

```
##   sigal_mean
## 1  -61.85237
```

```r
# compute standard deviation of signal strength
cleaned %>%
  summarise(signal_sd = sd(signal))
```

```
##   signal_sd
## 1  12.72555
```
