# Assignment 4 - Aliens Are Coming!! 
# ====================================

#' Note to User: Please ensure you have your working directly set accordingly.
#' This script makes use of the data 'ufo_subset.csv'. Ensure that this file
#' is in your working directory for the code to execute as expected. 
#' To read this script, you can click "Source" at the top right and go through
#' each block of code. For each section a new object has been created in the environment
#' so you can refer to the appropriate object in the "Environment" section. 

library(tidyverse) 

#' Note to user: this script uses the tidyverse package. Please ensure you have the
#' package installed prior to running this script. For further information, please
#' reference: https://www.tidyverse.org 

# Start of the Code

# ---- Loading the Data ----

#' Below we are loading the 'ufo_subset.csv' file as a data frame by using read_delim(). 
#' We pass in the argument delim="," to specify that this is a comma separated file.
#' Note: read_csv() or read_csv2() from Tidyverse could have been used, but we decided to
#' use the parent function read_delim() for the greatest parameter flexibility. 

raw.data <- read_delim("ufo_subset.csv", delim=",") 

#' Next we convert our raw data data frame to a tibble. 
#' We use .name_repair = 'universal' as it is tidyverse's automatic column name fixing standards.
#' This will ensure our column names do not have spaces or white spaces. In the console
#' you will see that 'duration seconds' and 'duration hours min' have been renamed with
#' periods instead of spaces, as these two original column names violated the tidyverse standards.

name.repaired <- as_tibble(raw.data, .name_repair = "universal")

#' A preview of the tibble 'name.repaired' is provided below (first 6 observations). 
#' This will be printed to console.
#' To see all data please refer to 'name.repaired' in the Environment tab.
print(head(name.repaired))

# ---- Fixing Missing Shape Information ----

#' Finding the rows where Shape information is missing and impute with "unknown".
#' Below we are creating a new tibble 'shapes.cleaned' that mutates our 'name.repaired' tibble.
#' Specifically, it mutates the shape value for all entries. If the value of shape is NA
#' the NA will be replaced with 'unknown'. If the value fo shape is not NA (default),
#' the existing value of shape will be preserved. 

shapes.cleaned <- name.repaired %>% 
  mutate(shape = case_when(
    is.na(shape) ~ "unknown", 
    .default = shape))

# Preview of the new unknown shape values is printed to console below (first 6 observations).
# To see all the data please look at 'shapes.cleaned' in the Environment tab.
print(head(shapes.cleaned %>% filter(shape == "unknown")))

# --- Removing Rows that Do Not Have Country Information ----
#' What we want to achieve:
#' Remove the rows that do not have Country information. If country name is missing but in city
#' column, extract it and put it in country column respectively.

#' Below:
#' 1) We are reating a new tibble 'fixed.country' that takes in the data from 'shapes.cleaned' tibble.
#' 2) We then use the mutate function. First we use case_when() to find observations 
#' where country is equal to the empty string or NA (with whitespace trimmed for more accuracy) 
#' AND where city values match the first regex pattern. This 'first regex pattern' is to check if
#' the city string contains parentheses. Essentially, this allows us to identify observations where
#' country is missing, but the respective city value has parentheses. This is because if the city 
#' contains parentheses we assume this contains the country name. This isn't the case for all
#' city values that have parentheses, but for the majority so we will make this generalization even though
#' it is not perfect. 
#' 3) For these cases, we then extract the city string that is inside of the parentheses using str_extract. 
#' The second regex allows us to get the values inside of the last set of parentheses in the city string. 
#' We created this regex to only get the letter strings inside of the last set of parentheses as some city strings 
#' have multiple parentheses. For example: '(Quebec) (Canada).
#' For these, the last one seemed to include the country name more frequently than the 
#' earlier sets of parentheses. As such, this will give us better accuracy. 
#' Note: thank you stackoverflow :).  
#' Note: The decision was made to only get letter strings using [a-z]*? as there are some parentheses 
#' that have non-letter text, which is definitely not a valid country name. 
#' 4) The steps 2-3 above allow us to populate some of the missing country fields with better data.
#' 5) If the conditions above were not met the .default = country line just keeps the country value
#' that was in place originally (e.g. for data where country data was good we preserve it)
#' 6) Lastly, we use filter() to now filter out any observations where the country value is NA or the
#' empty whitespace (after trimming, for better accuracy). This is because we want these incomplete
#' observations removed from our 'fixed.country' tibble.

fixed.country <- shapes.cleaned %>%
  mutate(country = case_when(
    (trimws(country) == "" | is.na(country) &
    grepl("\\(.*\\)", city)) ~ str_extract(city, "(?<=\\()([a-z]*?)(?=\\)[^()]*$)"),
    .default = country)) %>%
  filter(!(trimws(country) == "" | is.na(country)))

# Preview of the new fixed.country tibble is printed to console below (first 6 observations).
# To see all the data please look at 'fixed.country' in the Environment tab.
print(head(fixed.country))

# ---- Convert Datetime and Date_posted columns into appropriate formats ----

#' Below we use tidyverse's lubridate to convert character strings into date-time objects. 
#' Currently, we have 'datetime' and 'date_posted' has character strings. So we will conver them.
#' The format for datetime and date_posted was identifying by looking at the nature of the data as
#' we want to ensure that the date-time objects are parsed correctly.
#' These mutations have been assigned to a new tibble called 'fixed.time'. 
#' The ymnd_hm() and dmy() functions are from lubridate and allow us to convert the strings
#' to the appropritate date-time objects. 

fixed.time <- fixed.country %>%
  mutate(datetime = ymd_hm(datetime)) %>%
  mutate(date_posted = dmy(date_posted))

# Preview of the new fixed.time tibble is printed to console below (first 6 observations).
# To see all the data please look at 'fixed.time' in the Environment tab.
print(head(fixed.time))

#  ---- Creating a filter to identify possible hoax reports. ----

#' We want to create a new boolean column "is_hoax" in the tibble to classify each observation as hoax or not.
#' For this task some assumptions were made:
#' First, the 'comments' variable in our UFO data contains some observations where NUFORC officials 
#' add a HOAX?? or HOAX tag. Both (with question marks and without) were classified as possible hoax reports. 
#' Second, there are some other observations where NUFORC comments include plausible alternatives such as 
#' "twinkling stars" or "possibly Venus". However, if there was no HOAX or HOAX?? tag, this was not classified as 
#' a possible hoax report as there is no certainty that this observation was HOAX or not. 

#' In the function below, a new tibble 'hoax.data' is created that takes in the data from
#' 'fixed.country' tibble. A new column (variable) 'is_hoax" is created. The value for each observation
#' in this 'is_hoax' column is either TRUE or FALSE based on the output from the grepl() function.
#' Explanation of GREPL function: The GREPL will match the Regex if the following pattern is found in comments:
#' 1) There are exactly two opening parentheses (( 
#' 2) After these parentheses, any non-new line character can appear. Optional.
#' 3) The string HOAX must then appear.
#' 4) After this, a question marke (or two) can appear this is optional and the count is not restricted.
#' 5) After the question mark any other non-new line character can appear. Optional.
#' 6) Closing parentheses can then appear ")" from 0 to 2 times. Justification: some HOAX notes did not
#' have the parentheses closed. 
#' Goal: This function returns TRUE if "HOAX" appears anywhere in the comment string for an observation.
#' Note: this regex pattern is matched to toupper() of comments, such that it is all capitalized. This
#' ensures we do not miss some of the special cases where NURFOC hoax notes were added in a non-capitalized
#' format. 
hoax.data <- fixed.time %>%
  mutate(is_hoax = case_when(
    #grepl("HOAX", comments, fixed = TRUE) ~ TRUE,
    grepl("\\({2}.{0,}HOAX\\?*.{0,}\\){0,2}", toupper(comments), fixed = F) ~ TRUE,
    .default = FALSE
  ))

# Preview of the new hoax.data tibble is printed to console below (first 6 observations).
# To see all the data please look at 'hoax.data' in the Environment tab.
print(head(hoax.data))

# ---- Create a table reporting the percentage of hoax sightings per country. ---- 

#' To achieve this, we have created a new tibble 'hoax.country' which takes in the data from 'hoax.data'.
#' First we group by the countries. Then we create a new variable 'Hoax.Percentage'.
#' This is equal to the sum of hoax sightings in a country divided by the total sightings in a country
#' times 100 (to get the percentage value). Lastly, we arrange the hoax.country in descending
#' order by the Hoax.Percentage This shows the highest ratio countries first.

hoax.country <- hoax.data %>%
  group_by(country) %>%
  summarise(Hoax.Percentage = ((sum(is_hoax, na.rm=T)/n()) * 100)) %>%
  arrange(desc(Hoax.Percentage))

# Preview of the table showing percentage of hoax sightings per country.
# This is printed to console below (first 6 observations).
# To see all the data please look at 'hoax.country' in the Environment tab.
print(head(hoax.country))

# ---- Adding another column to the dataset (report_delay) and populate with the time difference 
# in days, between the date of the sighting and the date it was reported.  ---- 

#' Below we are creating a new tibble called 'time.diff'. This takes in 'hoax.data' which is
#' the un-summarized tibble from before (we don't want to use the grouped country table hoax.country as
#' we want the time differences in days for each observation).
#' We create a new variable (column) 'report_delay' which calculates the difference of the date_posted
#' and datetime (of sighting) for each observation in days (as the unit).

#' ASSUMPTION MADE: the difftime() gave days with decimal values. However our unit is in days so we
#' decided to round the result. The assumption made was that days were rounded up to the nearest full day.
#' Justification: Suppose a sighting was reported on June 11 at 11:30 PM. If it was reported on June 12th,
#' the difftime() result would be < 1. However, in terms of the calendar days the two events did not happen
#' on the same day, hence we round up using the ceiling() function.
time.diff <- hoax.data %>% 
  mutate(report_delay = 
           ceiling(difftime(date_posted, datetime, units = "days")))

# Preview of the new time.diff tibble is printed to console below (first 6 observations).
# To see all the data please look at 'time.diff' in the Environment tab.
print(head(time.diff))


# ---- Remove the rows where the sighting was reported before it happened. ----

#' The instances were a sighting was reported before it happened is the instances
#' where date_posted occurs before datetime. From the 'time.diff' calculations above,
#' this would mean that the report_delay is < 0 days. As such, we will remove the rows
#' where report_delay is less than 0. The new tibble is 'real.times'. We use the
#' filter() function to keep the items where report_delay is NOT less than 0.

real.times <- time.diff %>%
  filter(!(report_delay < 0))

# Preview of the new real.times tibble is printed to console below (first 6 observations).
# To see all the data please look at 'real.times' in the Environment tab.
print(head(real.times))

# ---- Create a table reporting the average report_delay per country. ---- 

#' To achieve this, we create a new tibble called 'report.delay.country' which takes in
#' the data from 'real.times'. We then group the observations by country using group_by().
#' For each country we then summarise the 'Average.Report.Delay' which is the mean of the
#' report_delay for each observation. Lastly, we arrange the table to show the average report
#' delay for each country in descending order. 
report.delay.country <- real.times %>%
  group_by(country) %>%
  summarise(Average.Report.Delay = mean(report_delay)) %>%
  arrange(desc(Average.Report.Delay))

# Preview of the 'report.delay.country' tibble is printed to console below (first 6 observations).
# To see all the data please look at 'report.delay.country' in the Environment tab.
print(head(report.delay.country))


# ---- Data Quality of: duration seconds column ----

# Doing some basic analysis of the duration seconds data below. This will show us
# the minimum value, the maximum value, and the number of NA values. 
print(paste("Minimum Value for Duration Seconds:", min(real.times$duration.seconds)))
print(paste("Maximum Value for Duration Seconds:", max(real.times$duration.seconds)))
print(paste("Number of NA values:", sum(is.na(real.times$duration.seconds))))

#' From the minimum and maximum values we an see that the range of our data is very large.
#' This will give us problems when trying to make a histogram... But what kind of problems?

summary(real.times$duration.seconds)

#' The summary function above shows us that we have a median of 180 seconds and an interquartile
#' range of around 555 seconds. Additionally our 75th percentile is 600 seconds, such that 75% of
#' the observed data falls below this value. However, our maximum value is huge. So this shows us
#' we have some extreme values on the right side of our data distribution. These seem to be skewing
#' our distribution to the right, which is also shown as the mean is substantially larger than the mean.
#' We could remove these extreme observations, such that observations with a duration in seconds
#' greater than some threshold are removed. However, we cannot arbitrarily determine what a reasonable
#' threshold is. For all we know, the  high duration in second observations could be legitimate data points.
#' As such, we are not going to change or edit any individual observation and compromise the integrity
#' of our data. Rather, we will apply a logarithm transformation to the duration second variable to counteract
#' this positive skew. This transformation will allow us to keep the relative relationships between our
#' observations in place, but also reduce the extreme range between our data points there are preventing us
#' from making meaningful insights. 

# As such, we will create a new variable: log.duration.seconds whichs applies the log10 transformation
# to the duration.seconds variable. We will store this in tibble 'final.data'. However, as shown below
# we have some values where log10() would give us a negative value. These are observations where the
# duration.seconds is < 1. As such, we will first apply a ceiling function to these observations to
# prevent having negative log10 numbers. This is the only manipulation we will do to our raw data.

final.data <- real.times %>%
  mutate(duration.seconds = case_when(
    duration.seconds < 1 ~ 1,
    .default = duration.seconds
  )) %>%
  mutate(log.duration.seconds = log10(duration.seconds))
  

# Histogram of Duration Seconds Variable
hist(final.data$log.duration.seconds, xlab = c("Log10() of Duration in Seconds"), 
     main = c("Histogram: Frequency of Observation Duration"))

#' As you can see from this histogram, the log10() transformation addressed the right skew of our
#' extreme values. However, the x-axis isnow the log10() of the duration in seconds for each observation.
#' The issue with this is that log10() of duration in seconds is not easily analyzed by humans and thus
#' has limited value for analysis. To address this, let's take another approach. We will transform our continuous
#' duration seconds variable to categorical bins. We will do this using case_when statements. See below:

final.data.bins <- final.data %>%
  mutate(duration.bin = case_when(
    duration.seconds > 0 & duration.seconds < 10 ~ "1-10 Seconds",
    duration.seconds >= 10 & duration.seconds < 30 ~ "10-30 Seconds",
    duration.seconds >= 30 & duration.seconds < 60 ~ "30-60 Seconds",
    duration.seconds >= 60 & duration.seconds < 300 ~ "60-300 Seconds",
    duration.seconds >= 300 & duration.seconds < 600 ~ "300-600 Seconds",
    duration.seconds >= 600 & duration.seconds < 3600 ~ "600 Seconds - 3600 Seconds",
    duration.seconds >= 3600 ~ "More than 3600 Seconds"
  ))

# We can now plot these bins using a barplot. This will show the frequency breakdown per bin, which is
# easier to interpret. 
barplot(table(final.data.bins$duration.bin), ylab = c("Frequency of Observation"),
        xlab = c("Observation Duration in Seconds"), cex.names=0.5, 
        main=c("Frequency of Observation Duration in Seconds"))

