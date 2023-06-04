library(dplyr)
library(stringr)

school_df <- read.csv("Public_School.csv")

food_df <- read.csv("Food_Access.csv")

population_df <- read.csv("County_Population_Data.csv")

poverty_df <- read.csv("Poverty_Metro_Data.csv")

#Public School Variables:

#TOTAL = total enrollment 
#TOTFRL= number of free lunch students
#FTE = total teachers
#STUTERATIO = student/teacher ratio
#SCHOOL_TYPE_TEXT: Regular school, Alternative Education School, Career and Technical School, Special education school
#TITLEI = whether the school is Title 1 eligible. Binary, either "2-No" or "1-Yes"
#CHARTER_TEXT = whether charter. Binary, either "No" or "Yes"
#MAGNET_TEXT = whether magnet. Binary, either "No" or "Yes"

#AM = number of Native American students
#AS = number of Asian students
#BL = number of black students
#HP = number of native Hawaiian/ PI students
#HI = number of Hispanic students
#TR = all students, two or more races
#WH = number of white students

#ULOCALE : county type (broadly divided into Suburb, Rural, Town, City)

#Food Access Variables:

#LACCESS_CHILD15 = Children, low access to store, 2015 (Count)
#PCT_LACCESS_CHILD15 = Children, low access to store (%), 2015	
#PCH_LACCESS_CHILD_10_15 = Children, low access to store (% change from 2010 - 15)
#LACCESS_SNAP15 = SNAP households, low access to store, 2015 (Count)
#PCT_LACCESS_SNAP15	= SNAP households, low access to store (%), 2015 (Percent)
#PCT_LACCESS_HHNV15 = Households, no car & low access to store (%), 2015

school_df <- filter(school_df, !is.na(TOTAL))

school_df <- school_df[school_df$TOTAL != 0, ]
       
colnames(school_df)[30]="County"

colnames(school_df)[6]="State"

colnames(school_df)[29]="County_Type"

school_df[str_detect(school_df$County_Type, "Rural") == TRUE, "County_Type"] <- "Rural"

school_df[str_detect(school_df$County_Type, "City") == TRUE, "County_Type"] <- "City"

school_df[str_detect(school_df$County_Type, "Town") == TRUE, "County_Type"] <- "Town"

school_df[str_detect(school_df$County_Type, "Suburb") == TRUE, "County_Type"] <- "Suburb"

grouped <- group_by(school_df, County, State)

df_2 <- summarize(grouped, num_student = sum(TOTAL))

#Making a County Type Variable, Based on Where a Plurality of Kids Go to School:

find_county_type <- function(county, state) {
our_county <- grouped[grouped$County == county & grouped$State == state, ]
rural_students <- sum(our_county[our_county$County_Type == "Rural", ] $TOTAL)
town_students <- sum(our_county[our_county$County_Type == "Town", ] $TOTAL)
suburb_students <- sum(our_county[our_county$County_Type == "Suburb", ] $TOTAL)
city_students <- sum(our_county[our_county$County_Type == "City", ] $TOTAL)
type_vec <- c(rural_students, town_students, suburb_students, city_students)
names(type_vec) <- c("Rural", "Town", "Suburb", "City")
max <- head(sort(type_vec, decreasing=TRUE), n=4)
max_type <- noquote(names(max[1]))
return(max_type)
}

df_2$County_Type <- mapply(find_county_type, df_2$County, df_2$State)

df_3 <- summarize(grouped, num_free_lunch = sum(ifelse(is.na(TOTFRL), NA, TOTFRL)))

df_4 <- inner_join(df_2, df_3, by = c("County", "State")) 

percent_free_lunch <- ifelse(
  is.na(df_4$num_free_lunch) | df_4$num_student == 0,
  NA,
  (df_4$num_free_lunch / df_4$num_student) * 100
)

percent_free_lunch <- round(percent_free_lunch, 2)

df_4$percent_free_lunch <- percent_free_lunch

df_5 <- summarize(grouped, num_white_students = sum(WH))

df_6 <- inner_join(df_4, df_5, by = c("County", "State"))

perc_non_hispanic_white_students <- ifelse(
  is.na(df_6$num_white_students),
  NA,
  (df_6$num_white_students / df_6$num_student) * 100
)

perc_non_hispanic_white_students <- round(perc_non_hispanic_white_students, 2)

df_6$perc_non_hispanic_white_students <- perc_non_hispanic_white_students

df_7 <- summarize(grouped, student_teacher_ratio = sum(FTE) / sum(TOTAL) * 100)

df_8 <- inner_join(df_6, df_7, by = c("County", "State"))

#Making a Percent Title 1 Eligible Variable:

Title_1_or_not <- function(county, state) {
our_county <- grouped[grouped$County == county & grouped$State == state, ]
if (any(str_detect(our_county$TITLEI, "M"))) {
  percent_title <- NA
} else {
num_title <- str_detect(our_county$TITLEI, "1-Yes")
num_title <- sum(num_title) / nrow(our_county)
percent_title <- num_title * 100 
return(percent_title)
}
}

df_8$Percent_Title_1 <- mapply(Title_1_or_not, df_8$County, df_8$State)

df_8$Percent_Title_1 <- round(df_8$Percent_Title_1, 2)

#Making a Percent Magnet or Charter Variable:

magnet_charter_or_not <- function(county, state) {
  our_county <- grouped[grouped$County == county & grouped$State == state, ]
  num_magnet <- str_detect(our_county$MAGNET_TEXT, "Yes")
  num_charter <- str_detect(our_county$CHARTER_TEXT, "Yes")
  num_both <- num_magnet + num_charter
  percent_either <- sum(num_both) / nrow(our_county)
  percent_either <- percent_either * 100 
  return(percent_either)
}

df_8$Percent_Charter_or_Magnet <- mapply(magnet_charter_or_not, df_8$County, df_8$State)

df_8$Percent_Charter_or_Magnet <- round(df_8$Percent_Charter_or_Magnet, 2)

#Cleaning Up County Names:

school_county <- gsub('County',' ',df_8$County)

df_8$County <- school_county

school_county <- gsub('  ', '',df_8$County)

df_8$County <- school_county

joined_df <- inner_join(df_8, food_df, by = c("County", "State"))

#Deleting Unnecessary Num White Students Column:

joined_df <- select(joined_df, -7)

#Renaming (and Deleting) Columns in the Joined Dataframe:

joined_df <- select(joined_df, -17,-20,-22,-25,-29,-34:-38,-41:-50)

joined_df <- select(joined_df, -31,-32)

colnames(joined_df)[12]="Low Access to Store (Count, 2010)"

colnames(joined_df)[13]="Low Access to Store (Count, 2015)"

colnames(joined_df)[14]="Population, Low Access to Store (% change, 2010-15)"

colnames(joined_df)[15]="Population, low access to store (%), 2010"

colnames(joined_df)[16]="Population, low access to store (%), 2015"

colnames(joined_df)[17]="Low income & low access to store, 2015"

colnames(joined_df)[18]="Low income & low access to store (% change), 2010 - 15"

colnames(joined_df)[19]="Low income & low access to store (%), 2015"

colnames(joined_df)[20]="Households, no car & low access to store, 2015"

colnames(joined_df)[21]="Households, no car & low access to store (% change), 2010 - 15"

colnames(joined_df)[22]="Percent Household No Car, Low Access to Store (2015)"

colnames(joined_df)[23]="SNAP households, low access to store, 2015"

colnames(joined_df)[24]="Percent SNAP Household, Low Access to Store (2015)"

colnames(joined_df)[25]="Children, low access to store, 2015"

colnames(joined_df)[26]="Children, Low Access to Store (% Change from 2010-15)"

colnames(joined_df)[27]="Percent Children Low Access to Store (2010)"

colnames(joined_df)[28]="Percent Children Low Access to Store (2015)"

colnames(joined_df)[29]="Whites, Low Access to Store (2015)"

colnames(joined_df)[30]="Percent Whites, Low Access to Store (2015)"

joined_df$`Percent Children Low Access to Store (2010)` <- round(joined_df$`Percent Children Low Access to Store (2010)`, 0)

joined_df$`Population, low access to store (%), 2010` <- round(joined_df$`Population, low access to store (%), 2010`, 0)

joined_df$`Low Access to Store (Count, 2010)` <- round(joined_df$`Low Access to Store (Count, 2010)`, 0)

joined_df$`Low Access to Store (Count, 2015)`<- round(joined_df$`Low Access to Store (Count, 2015)`, 0)

joined_df$`Low income & low access to store, 2015` <- round(joined_df$`Low income & low access to store, 2015`, 0)

joined_df$`Population, Low Access to Store (% change, 2010-15)` <- round(joined_df$`Population, Low Access to Store (% change, 2010-15)`, 2)

joined_df$`Population, low access to store (%), 2015`<- round(joined_df$`Population, low access to store (%), 2015`, 2)

joined_df$`Low income & low access to store (% change), 2010 - 15` <- round(joined_df$`Low income & low access to store (% change), 2010 - 15`, 2)

joined_df$`Low income & low access to store (%), 2015` <- round(joined_df$`Low income & low access to store (%), 2015`, 2)

joined_df$`Households, no car & low access to store, 2015` <- round(joined_df$`Households, no car & low access to store, 2015`, 0)

joined_df$`Households, no car & low access to store (% change), 2010 - 15` <- round(joined_df$`Households, no car & low access to store (% change), 2010 - 15`, 2)

joined_df$`SNAP households, low access to store, 2015` <- round(joined_df$`SNAP households, low access to store, 2015`, 0)

joined_df$`Percent Household No Car, Low Access to Store (2015)` <- round(joined_df$`Percent Household No Car, Low Access to Store (2015)`, 2)

joined_df$`Percent SNAP Household, Low Access to Store (2015)` <- round(joined_df$`Percent SNAP Household, Low Access to Store (2015)`, 2)

joined_df$`Children, low access to store, 2015` <- round(joined_df$`Children, low access to store, 2015`, 0)

joined_df$`Whites, Low Access to Store (2015)` <- round(joined_df$`Whites, Low Access to Store (2015)`, 0)

#Merging with supplementary population data from the Food Environment Atlas:

joined_df <- left_join(joined_df, population_df, by = "FIPS")

joined_df <- select(joined_df, -31,-32)

colnames(joined_df)[1]="County"

colnames(joined_df)[2]="State"

joined_df <- left_join(joined_df, poverty_df, by = "FIPS")

joined_df <- select(joined_df, -34,-35)

colnames(joined_df)[1]="County"

colnames(joined_df)[2]="State"

#Making a Direction of Change in Food Access Variable:

pop_direction_of_change <- function(county, state) {
  our_county <- filter(joined_df, County == county & State == state)
  direction <- ifelse(is.na(our_county$`Population, Low Access to Store (% change, 2010-15)`),
                      "No_Value_Given",
                      ifelse(our_county$`Population, Low Access to Store (% change, 2010-15)` > 0,
                             "Negative",
                             ifelse(our_county$`Population, Low Access to Store (% change, 2010-15)` < 0,
                                    "Positive",
                                    "No_Change")))
  return(direction)
}

joined_df$pop_food_access_direction_of_change <- mapply(pop_direction_of_change, joined_df$County, joined_df$State)

joined_df$pop_food_access_direction_of_change <- sapply(joined_df$pop_food_access_direction_of_change, function(x) paste(x, collapse = ","))

is_rural_urban <- function(number) {
  if (number == 0) {
    rural_urban <- "rural"
    return(rural_urban)
  } else if (number == 1) {
    rural_urban <- "urban"
    return(rural_urban)
  }
}

joined_df$rural_urban <- mapply(is_rural_urban, joined_df$METRO13)

#Making an Above or Below Child Poverty Variable:

avg_child_pov <- mean(joined_df$CHILDPOVRATE15, na.rm = TRUE)

above_avg_child_poverty <- function(number) {
  if (is.na(number)) {
    above_avg_child_pov <- "NA"
    return(above_avg_child_pov)
  } else if (number > avg_child_pov) {
    above_avg_child_pov <- "Yes"
    return(above_avg_child_pov)
  } else {
    above_avg_child_pov <- "No"
    return(above_avg_child_pov)
  }
}

joined_df$above_avg_child_pov <- mapply(above_avg_child_poverty, joined_df$CHILDPOVRATE15)

#Making a Food Desert Variable:

food_desert_df <- filter(joined_df, !is.na(`Population, low access to store (%), 2015`))

avg_low_access <- mean(food_desert_df$`Population, low access to store (%), 2015`)

is_food_desert <- function(number) {
  if (is.na(number)) {
    food_desert <- "NA"
    return(food_desert)
  } else if (number > avg_low_access) {
    food_desert <- "Yes"
    return(food_desert)
  } else {
    food_desert <- "No"
    return(food_desert)
  }
}

joined_df$food_desert <- mapply(is_food_desert, joined_df$`Population, low access to store (%), 2015`)

#Creating a Relative Title 1 Variable:
title_1_df <- filter(joined_df, !is.na(Percent_Title_1))

avg_title_1 <- mean(title_1_df$Percent_Title_1)

title_1_function <- function(number) {
  if(is.na(number)) {
    relative_title_1 <- NA
  } else if (number < (avg_title_1 * 0.5)) {
    relative_title_1 <- "Low_Title_1_Funding"
    return(relative_title_1)
  } else if (number == 100.00) {
    relative_title_1 <- "100%_Title_1_Eligible"
    return(relative_title_1)
  } else {
    relative_title_1 <- NA
    return(relative_title_1)
  }
}

joined_df$relative_title_1 <- mapply(title_1_function, joined_df$Percent_Title_1)

write.csv(joined_df, "join.csv")

#Creating a Kings County Dataframe For the Purposes of our Shiny App:

kings_county <- joined_df[joined_df$County == "Kings" & joined_df$State == "NY", ]

write.csv(kings_county, "kings_county.csv")
