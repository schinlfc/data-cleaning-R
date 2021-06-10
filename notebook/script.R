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


### Section II

#### Load tax dataset

setwd('/Users/schinlfc/data-science-R/data-cleaning-R/data/')
tax <- read_csv('real_property_taxes.csv')


##### 6. How many addresses pay property taxes?

dim(tax)
nrow(tax)
length(tax$PropertyID)
sum(is.na(tax$CityTax))
sum(!is.na(tax$CityTax))


##### 7. What is the total city and state tax paid?  
#You need to remove the `$` from the `CityTax` variable

###### 7.1 City

head(tax$CityTax) 
tax <- tax %>% 
  mutate(CityTax = str_replace(CityTax, fixed("$"), ""),
         CityTax = as.numeric(CityTax))
head(tax$CityTax)
tax$CityTax <- str_replace(tax$CityTax, fixed("$"), "")
tax$CityTax <- as.numeric(tax$CityTax)
head(tax$CityTax)
sum(is.na(tax$CityTax))
options(digits=12) # so no rounding
sum(tax$CityTax, na.rm = TRUE)
sum(tax$CityTax, na.rm = TRUE) / 1e6


###### 7.2 State

options(digits=12) # so no rounding
head(tax$StateTax)
tax <- tax %>% mutate(StateTax = parse_number(StateTax))
head(tax$StateTax)
sum(is.na(tax$StateTax))
sum(tax$StateTax, na.rm = TRUE)
sum(tax$StateTax, na.rm = TRUE) / 1e6


##### 8. Using `table()` or `group_by` and `summarize(n())` or `tally()`
###### 8.1 how many observations/properties are in each Ward?

table(tax$Ward)
ward_table <- tax %>% group_by(Ward) %>% tally()
print(ward_table)
ward_table <- tax %>% group_by(Ward) %>%
  summarise(number_of_observations = n())
print(ward_table)


###### 8.2 what is the mean state tax per Ward? use `group_by` and `summarize

mean_statetax <- tax %>% group_by(Ward) %>%
  summarise(mean_statetax = mean(StateTax, na.rm = TRUE))
print(mean_statetax)


###### 8.3 what is the maximum amount still due in each Ward?  different summarization (`max`)

tax <- tax %>% mutate(AmountDue = str_replace(
  AmountDue, fixed("$"), ""), AmountDue = as.numeric(AmountDue))
head(tax$AmountDue)

tax$AmountDue = tax$AmountDue %>% 
  str_replace(fixed("$"), "") %>%
  as.numeric
head(tax$AmountDue)

tax = tax %>% mutate(AmountDue = as.numeric(str_replace(
  AmountDue, fixed("$"), "")))
head(tax$AmountDue)

max_due <- tax %>% group_by(Ward) %>% 
  summarise(max_amount_due = max(AmountDue, na.rm = TRUE))
print(max_due)


###### 8.4 what is the 75th percentile of city and state tax paid by Ward? (`quantile`)

percentile_city <- tax %>% group_by(Ward) %>% 
  summarise(percentile_city = quantile(
    CityTax, prob = 0.75, na.rm = TRUE))
print(percentile_city)

percential_state <- tax %>% group_by(Ward) %>%
  summarise(percential_state = quantile(
    StateTax, prob = 0.75, na.rm = TRUE))
print(percential_state)

percentile_city_state <- tax %>% 
  group_by(Ward) %>% summarise(
    percentile_city = quantile(CityTax, prob = 0.75, na.rm =TRUE),
    percentile_state = quantile(StateTax, prob = 0.75, na.rm =TRUE))
print(percentile_city_state)


##### 9. Make boxplots using showing `cityTax` (`y` -variable) by whether the property	is a principal residence (`x`) or not.

tax <- tax %>% mutate(CityTax = as.numeric(str_replace(CityTax, fixed("$"), "")),
                      ResCode = str_trim(ResCode))

ggplot(tax, aes(x=ResCode, y=log10(CityTax+1), color=ResCode)) +
  geom_boxplot() +
  labs(title="City tax by residential code") +
  xlab("Residential code") +
  ylab("City tax")


##### 10. Subset the data to only retain those houses that are principal residences. Which command subsets rows? Filter or select?

###### 10.1 How many such houses are there?

pres = tax %>% filter( ResCode %in% "PRINCIPAL RESIDENCE")
nrow(pres)
pres = tax %>% filter( ResCode == "PRINCIPAL RESIDENCE")
nrow(pres)


###### 10.2 Describe the distribution of property taxes on these residences.

pres <- pres %>% mutate(StateTax = as.numeric(str_replace(StateTax, fixed("$"), "")))

ggplot(pres, aes(x=log(CityTax))) + 
  geom_histogram(binwidth=0.3, color="darkblue",
                 fill="lightblue")

ggplot(pres, aes(x=log(CityTax)))+
  geom_density(color="darkblue", fill="lightblue")

ggplot(pres, aes(x=log(StateTax))) + 
  geom_histogram(binwidth=0.3, color="darkblue",
                 fill="lightblue")

ggplot(pres, aes(x=log(StateTax)))+
  geom_density(color="darkblue", fill="lightblue")


### Section III

#### Load salary dataset

setwd('/Users/schinlfc/data-science-R/data-cleaning-R/data/')
sal <- read_csv('baltimore_city_employee_salaries_FY2015.csv')


##### 11. Make an object called health.sal using the salaries data set, with only agencies of those with `"fire"` (or any forms), if any, in the name remember `fixed( ignore_case = TRUE)` will ignore cases.

health.sal <- sal %>% filter(str_detect(Agency, fixed("fire", ignore_case = TRUE)))
print(head(health.sal))


##### 12. Make a data set called `trans` which contains only agencies that contain "TRANS".

trans <- sal %>% filter(str_detect(Agency, "TRANS"))
print(head(trans))


##### 13. What is/are the profession(s) of people who have `"abra"` in their name for Baltimore's Salaries?  Case should be ignored. 

print(sal %>% 
        filter(str_detect(name, fixed("abra", ignore_case = TRUE))) %>% select(name, JobTitle))


##### 14. What is the distribution of annual salaries look like? (use `hist`) What is the IQR? Hint: first convert to numeric. Try `str_replace`, but remember`$` is "special" and you need `fixed()` around it.

sal <- sal %>% 
  mutate(AnnualSalary = as.numeric(
    str_replace(AnnualSalary, fixed("$"), "")))
print(head(sal))

ggplot(sal, aes(x=log(AnnualSalary))) + 
  geom_histogram(binwidth=0.3, color="darkblue",
                 fill="lightblue")

quantile(sal$AnnualSalary)


##### 15. Convert `HireDate` to the `Date` class - plot Annual Salary vs Hire Date. Use `AnnualSalary ~ HireDate` with a `data = sal` argument in plot or use x, y notation in `scatter.smooth`

sal <- sal %>% mutate(HireDate = lubridate::mdy(HireDate))

ggplot(sal, aes(HireDate, AnnualSalary)) +
  geom_point() +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE,
              level=0.95)


##### 16. Create a smaller dataset that only includes the Police Department,  Fire Department and Sheriff's Office.  Use the Agency variable with string matching. Call this `emer`.  How many employees are in this new dataset?

emer <- sal %>% filter(str_detect(
  Agency, "Police Department | Fire Department | 
  Sheriff's Office"))
nrow(emer) # This will only show Police Department

emer <- sal %>% filter(
  str_detect(Agency, "Police Department") |
    str_detect(Agency, "Fire Department") |
    str_detect(Agency, "Sheriff's Office"))
nrow(emer) # This will show everything


##### 17. Create a variable called `dept` in the emer data set.

emer <- emer %>% mutate(dept = str_extract(
  Agency, ".*(ment|ice)"))

ggplot(emer, aes(HireDate, AnnualSalary, color=dept)) +
  geom_point()


##### 18. (Bonus). Convert the 'LotSize' variable to a numeric square feet variable in the tax data set.

tax$LotSize = str_trim(tax$LotSize) # trim to be safe
lot = tax$LotSize

# First lets take care of acres
aIndex= which(str_detect(tax$LotSize, "AC.*") |
                str_detect(tax$LotSize, fixed(" %")))
print(head(aIndex))

print(head(lot[aIndex]))

acre = tax$LotSize[aIndex] # temporary variable
## find and replace character strings
acre = str_replace_all(acre, " AC.*","")
acre = str_replace_all(acre, " %","")
table(!is.na(as.numeric(acre)))

head(acre[is.na(as.numeric(acre))],50)

## lets clean the rest
acre = str_replace_all(acre, "-",".") # hyphen instead of decimal
head(acre[is.na(as.numeric(acre))])

table(!is.na(as.numeric(acre)))

acre = str_replace_all(acre, "ACRES","")
head(acre[is.na(as.numeric(acre))])

# take care of individual mistakes
acre = str_replace_all(acre, "O","0") # 0 vs O
acre = str_replace_all(acre, "Q","") # Q, oops
acre = str_replace_all(acre, ",.",".") # extra ,
acre = str_replace_all(acre, ",","") # extra ,
acre = str_replace_all(acre, "L","0") # leading L
acre = str_replace_all(acre, "-",".") # hyphen to period
acre[is.na(as.numeric(acre))]

acre2 = as.numeric(acre)*43560 

sum(is.na(acre2)) # all but 3


#Now let’s convert all of the square feet variables

library(purrr)
fIndex = which(str_detect(tax$LotSize, "X"))

ft = tax$LotSize[fIndex]

ft = str_replace_all(ft, fixed("&"), "-")
ft = str_replace_all(ft, "IMP ONLY ", "")
ft = str_replace_all(ft, "`","1")

ft= map_chr(str_split(ft, " "), first)

## now get the widths and lengths
width = map_chr(str_split(ft,"X"), first)
length = map_chr(str_split(ft,"X"), nth, 2) 

## width
widthFeet = as.numeric(map_chr(str_split(width, "-"), first))

widthInch = as.numeric(map_chr(str_split(width, "-"),nth,2))/12
widthInch[is.na(widthInch)] = 0 # when no inches present
totalWidth = widthFeet + widthInch # add together

# length
lengthFeet = as.numeric(map_chr(str_split(length, "-"),first))
lengthInch = as.numeric(map_chr(str_split(length, "-",2),nth,2))/12


lengthInch[is.na(lengthInch)] = 0 # when no inches present
totalLength = lengthFeet + lengthInch

# combine together for square feet
sqrtFt = totalWidth*totalLength 
ft[is.na(sqrtFt)] # what is left?


#And now we combine everything together:

tax$sqft = rep(NA)
tax$sqft[aIndex] = acre2
tax$sqft[fIndex] = sqrtFt
mean(!is.na(tax$sqft))

# already in square feet, easy!!
sIndex=which(str_detect(tax$LotSize, "FT") | str_detect(tax$LotSize, "S.*F."))
sf = tax$LotSize[sIndex] # subset temporary variable

sqft2 = map_chr(str_split(sf,"( |SQ|SF)"),first)
sqft2 = as.numeric(str_replace_all(sqft2, ",", "")) # remove , and convert

tax$sqft[sIndex] = sqft2
table(is.na(tax$sqft))

## progress!
#what remains?
lot[is.na(tax$sqft)]

