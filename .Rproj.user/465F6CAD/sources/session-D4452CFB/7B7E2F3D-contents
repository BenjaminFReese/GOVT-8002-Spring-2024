# Lab II: Panel Data

```{r, echo=FALSE}
knitr::include_graphics("Images/image_2.png")
```

### Preparation

```{r, eval = FALSE}
# Load packages used in this session of R
library(knitr)
library(tidyverse)
library(plm)
library(readr)
library(readxl) # Package to read Excel data
opts_chunk$set(echo = TRUE)
options(digits = 2)

```

In this lab we will estimate standard panel data models on covid policy and cases/deaths in U.S. states.  This is not a full-fledged analysis, but rather an initial exploration of the data that illustrates how fixed effects models work.

## Load the data from Oxford_Covid_data_US_latest.csv

Oxford provides data on covid deaths/cases and policy variables by day by state. For more background, see this [data archive](https://www.bsg.ox.ac.uk/research/research-projects/coronavirus-government-response-tracker#data) or this [story](https://www.nytimes.com/interactive/2020/11/18/us/covid-state-restrictions.html) that uses the data.
                                                                                                                              
                                                                                                                              We will use a variable called _GovernmentResponseIndex_.  For details, see [this](https://www.bsg.ox.ac.uk/sites/default/files/2020-08/2020-08-06%20Oxford%20COVID-19%20Government%20Response%20Tracker%20expanded%20to%20US%20states%20PRESS%20RELEASE.pdf).  (And feel free to experiment with the other measures.)
                                                                                                                              
                                                                                                                              
                                                                                                                              ##  Data organization
                                                                                                                              - Load the _Oxford_Covid_data_US_latest.csv_ data
                                                                                                                              - Limit it to U.S. states (CountryName == "United States)
- Create the following variables: RegionName, RegionCode, Date, GovernmentResponseIndex, ConfirmedCases and ConfirmedDeaths
- Add a variable to this data frame using the following code (this variable will help us when merging below)
```{r eval = FALSE}
  mutate("stAbbrev" = str_replace_all(string = RegionCode, pattern = "US_", replacement = "" ))
```

- Show the first three variables of the first three lines of this data frame.
```{r tidy = FALSE}
# Load and filter state stringency data
stPolicy = read_csv("Data/Oxford_Covid_data_US_latest.csv") %>%
  filter(CountryName == "United States" & 
           is.na(RegionName) == 0 & 
           RegionName != "" &
           RegionCode != "US_VI") %>% 
  dplyr::select(RegionName, RegionCode, Date, ContainmentHealthIndex,
         GovernmentResponseIndex, StringencyIndex, 
         ConfirmedCases, ConfirmedDeaths) %>%
  mutate("stAbbrev" = str_replace_all(string = RegionCode, pattern = "US_", replacement = "" ))

# Show the first three variables of first three lines
stPolicy[1:3, 1:3]
```

## Use the lag function in dplyr to create lagged variables for cases and deaths.  Also create "difference" (e.g., dCases) that is the change in cases for each state by date.

See slide "Creating differenced data in R" in Topic 3 class slides.

```{r tidy = FALSE}
## Create lagged value 
stPolicy = stPolicy %>% 
  group_by(RegionName) %>% 
  mutate(lagCases  = dplyr::lag(ConfirmedCases, order_by = Date),
         dCases    = ConfirmedCases - lagCases,
         lagDeaths = dplyr::lag(ConfirmedDeaths, order_by = Date),
         dDeaths   = ConfirmedDeaths - lagDeaths) %>%
  ungroup()

```

## Merge the above data frame to data in _USstates.xlsx_
- Merge by state abbreviation
- Create per capita measures of change in deaths and cases (e.g. "deathsPC" = 10000*dDeaths/pop2019).

Check your data by looking at level and lagged data for a given state for a few years.  The lagged data should match up to the previous period observation.

```{r tidy = FALSE}
# Load excel data
stFacts <- read_excel("Data/USstates.xlsx", sheet = "data")

# Merge with data frame and create per capita data
dfState <- stPolicy %>%
  left_join(stFacts, by = c("stAbbrev" = "stateAbbr")) %>% 
  mutate("deathsPC" = 10000*dDeaths/pop2019, 
         "casesPC"  = 10000*dCases/pop2019) 

# Check data
dfState %>% filter(RegionName == "California") %>% 
  dplyr::select(RegionName, Date, ConfirmedDeaths, lagDeaths, dDeaths, deathsPC) %>% 
  slice(245:248)

```

## Estimate a pooled model of total cases per capita as a function of state policy. Discuss.  

```{r tidy = FALSE}
ols.1 = lm(casesPC ~ GovernmentResponseIndex, data = dfState)
summary(ols.1)
```


## Estimate a one-way fixed effect model where the fixed effect is state. (Note that state is indicated in a variable called *RegionName*.) Estimate using both LSDV and the _de-meaned_ version in the plm package.  Can you identify a source of bias? 
```{r tidy = FALSE, warning = FALSE}

fe.1 = lm(casesPC ~ GovernmentResponseIndex + factor(RegionName), data = dfState)
#summary(fe.1)
coefficients(summary(fe.1))[1:2,]

fe.1plm = plm(casesPC ~ GovernmentResponseIndex, 
  data = dfState, 
  index = c("RegionName","Date"),
  model="within")
summary(fe.1plm)

```

## Estimate a two-way fixed effect model where the fixed effects are state and date. Estimate using both LSDV and the _de-meaned_ version in the plm package.  Does this model address the source of bias identified earlier?


```{r tidy = FALSE}

fe.2 = lm(casesPC ~ GovernmentResponseIndex + factor(RegionName) + factor(Date), data = dfState)
#summary(fe.2)
# Show non-fixed effect coefficients
coefficients(summary(fe.2))[1:2,]

fe.2plm = plm(casesPC ~ GovernmentResponseIndex, 
  data = dfState, 
  index = c("RegionName","Date"),
  model="within", 
  effect="twoways")
summary(fe.2plm)

```

### This is an initial analysis.  We would also want to  
- think through whether it is useful to control for days of the week (there is a well-known pattern in reporting across days of the week)
- assess the data for outliers (e.g, `min(dfState$casesPC, na.rm = TRUE)`)
- consider a lagged dependent variable
- For more, read [this](https://cdn.vanderbilt.edu/vu-my/wp-content/uploads/sites/2318/2020/05/11154933/Covid-DD_v2.pdf)