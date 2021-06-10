### Load required libraries
load_libraries <- function(){
  library(readr)
  library(tidyverse)
  library(dplyr)
  library(lubridate)
  library(broom)
  library(ggplot2)
  print('The libraries have been loaded.')
  }
load_libraries()

### Section I
#### Load bike dataset
setwd('/Users/schinlfc/data-science-R/data-cleaning-R/data/')
bike = read_csv('bike_lanes.csv')

##### 1. Inspect the entire dataset
str(bike) 
summary(bike) 
names(bike) 
ncol(bike) 
nrow(bike) 
sum(complete.cases(bike))
sum(!complete.cases(bike)) 


##### 2. Create a data set called `namat` which is equal to `is.na(bike)`. 
# What is the class of `namat`?  Run rowSums and colSums on `namat.` 
# These represent the number of missing values in the rows and columns of `bike`. 
# Don't print `rowSums`, but do a table of the `rowSums`. 
# Try `nabike = bike %>% mutate_all(is.na)`. Then try `nabike %>% summarise_all(sum)`.  
# What do you get?
namat <- is.na(bike)
class(namat)
table(rowSums(namat))
colSums(namat)
nabike <- bike %>% mutate_all(is.na)
print(nabike)
print(nabike %>% summarise_all(sum))


##### 3. Filter rows of bike that are NOT missing the `route` variable, 
# assign this to the object `have_route.`  Do a table of the `subType` 
# using `table`, including the missing `subType`s  Get the same frequency d
# istribution using `group_by(subType)` and `tally()`
have_route <- bike %>% filter(!is.na(route))
print(have_route)
table(bike$subType, useNA='always')
print(have_route %>% group_by(subType) %>% tally())
print(have_route %>%  count(subType))
print(have_route %>% group_by(subType) %>% summarize(n_obs = n()))
print(tally(group_by(have_route, subType)))
have_route = group_by(have_route, subType)
print(tally(have_route))


##### 4. Filter rows of bike that have the type `SIDEPATH` or `BIKE LANE` 
# using `%in%`.  Call it `side_bike.` Confirm this gives you the same number 
# of results using the `|` and `==`.

side_bike <- bike %>% filter(type %in% c('SIDEPATH', 'BIKE LANE'))
print(side_bike)
side_bike <- bike %>% filter(type == 'SIDEPATH' | type == 'BIKE LANE')
print(side_bike)


##### 5. Do a cross tabulation of the bike `type` and the number of lanes. 
# Call it `tab`.  Do a `prop.table` on the rows and columns margins. 
# Try `as.data.frame(tab)` or `broom::tidy(tab)`
tab <- table(bike_type = bike$type, number_of_lanes = bike$numLanes)
print(tab)
prop.table(tab, 1)
prop.table(tab, 2)
print(as.data.frame(tab))
