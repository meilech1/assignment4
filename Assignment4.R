# Assignment 4 - Aliens Are Coming!!

#* User information & loading packages / file info will go here. TBD
#*

# Start of the Code
library(tidyverse) 
# Tell user this script uses Tidyverse

# Below we are loading the 'ufo_subset.csv' file as a tibble and using 
# .name_repair = 'universal' to leverage tidyverse's automatic column name fixing standards.
# This will ensure our column names do not have spaces or whitespaces. In the console
# you will see that 'duration seconds' and 'duration hours min' have been renamed with
# periods instead of spaces, as these two original column names violated the tidyverse standards.
# We piped these two processes together to make the code more effiicent. 

raw.data <- read_delim("ufo_subset.csv", delim=",") %>%
  as_tibble(.name_repair = "universal")

#' A preview of the tibble 'raw.data' is provided below (first 6 observations). 
#' This will be printed to console.
#' To see all data please refer to 'raw.data' in the Environment tab.
print(head(raw.data))

#' Finding the rows where Shape information is missing and impute with "unknown".
#' Below we are creating a new tibble 'shapes.cleaned' that mutates our 'raw.data' tibble.
#' Specifically, it mutates the shape value for all entries. If the value of shape is NA
#' the NA will be replaced with 'unknown'. If the value fo shape is not NA (default),
#' the existing value of shape will be preserved. 

shapes.cleaned <- raw.data %>% 
  mutate(shape = case_when(
    is.na(shape) ~ "unknown", 
    .default = shape))

# Preview of the new unknown shape values is printed to console below (first 6 observations).
# To see all the data please look at 'shapes.cleaned' in the Environment tab.
print(head(shapes.cleaned %>% filter(shape == "unknown")))

#' Remove the rows that do not have Country information. If country name is missing but in city
#' column, extract it and put it in country column respectively.

#' Below:
#' 1) Creating a new tibble 'missing.country' that consists of all the observations where country is NA
#' or an empty string (whitepsace trimmed). 
#' 2) The second pipe is the logic for extracting country names for observations where a country name 
#' is present in the city column. We use the case_when function to identify city values where the 
#' first regex pattern is matched (in this case, contains parentheses). This is because iff the city 
#' contains parentheses we assume this contains the country name. This isn't the case for all
#' city values that have parentheses, but for the majority so we will make this generalization even though
#' it is not perfect. 
#' 3) Thus, if the first regex is matched, we then extract the string that is inside of
#' the parentheses using str_extract. The second regex allows us to get the values inside of the last
#' set of parentheses in the city string. We created this regex to only get the string inside of the
#' last set of parentheses as some city strings have multiple parentheses. For these, the last one
#' seemed to include the country name more frequently than the earlier sets of parentheses. As such,
#' this will give us better accuracy. 

missing.country <- shapes.cleaned %>%
  filter(trimws(country) == "" | is.na(country)) %>%
  mutate(country = case_when(
    grepl("\\(.*\\)", city) ~ str_extract(city, "(?<=\\()([^()]*?)(?=\\)[^()]*$)"),
    .default = NA
  ))


           



