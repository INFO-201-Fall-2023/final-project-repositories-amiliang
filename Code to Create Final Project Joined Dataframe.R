library(dplyr)
library(stringr)
library(testthat)

school_df <- read.csv("Public_School.csv")

food_df <- read.csv("Food_Access.csv")

#Public School Variables:

#TOTAL = total enrollment (includes adult education students tho...)
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
       
school_df$TOTFRL[is.na(school_df$TOTFRL)]

missing_lunch_status <- c(school_df$TOTFRL[is.na(school_df$TOTFRL)])

school_df[is.na(school_df$TOTFRL), "TOTFRL"] <- 0

colnames(school_df)[30]="County"

colnames(school_df)[6]="State"

colnames(school_df)[29]="County_Type"

school_df[str_detect(school_df$County_Type, "Rural") == TRUE, "County_Type"] <- "Rural"

school_df[str_detect(school_df$County_Type, "City") == TRUE, "County_Type"] <- "City"

school_df[str_detect(school_df$County_Type, "Town") == TRUE, "County_Type"] <- "Town"

school_df[str_detect(school_df$County_Type, "Suburb") == TRUE, "County_Type"] <- "Suburb"

grouped <- group_by(school_df, County, State)

df_2 <- summarize(grouped, num_student = sum(TOTAL))

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

df_3 <- summarize(grouped, num_free_lunch = sum(TOTFRL))

df_4 <- inner_join(df_2, df_3, by = c("County", "State")) 

percent_free_lunch <- (df_4$num_free_lunch / df_4$num_student) * 100

percent_free_lunch <- round(percent_free_lunch, 2)

df_4$percent_free_lunch <- percent_free_lunch

df_5 <- summarize(grouped, perc_non_hispanic_white_students = sum(WH) / sum(TOTAL) * 100)

df_5$perc_non_hispanic_white_students <- round(df_5$perc_non_hispanic_white_students, 2)

df_6 <- inner_join(df_4, df_5, by = c("County", "State"))

df_7 <- summarize(grouped, student_teacher_ratio = sum(FTE) / sum(TOTAL) * 100)

df_8 <- inner_join(df_6, df_7, by = c("County", "State"))

Title_1_or_not <- function(county, state) {
our_county <- grouped[grouped$County == county & grouped$State == state, ]
num_title <- str_detect(our_county$TITLEI, "1-Yes")
num_title <- sum(num_title) / nrow(our_county)
percent_title <- num_title * 100 
return(percent_title)
}

df_8$Percent_Title_1 <- mapply(Title_1_or_not, df_8$County, df_8$State)

df_8$Percent_Title_1 <- round(df_8$Percent_Title_1, 2)

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

#Renaming (and Deleting) Columns in the Joined Dataframe:
joined_df <- select(joined_df, -12,-15,-17,-20,-22,-25,-29,-32,-34:-38,-41:-50)

joined_df <- select(joined_df, -28:-29)

colnames(joined_df)[12]="Low Access to Store (Count, 2015)"

colnames(joined_df)[13]="Population, Low Access to Store (% change, 2010-15)"

colnames(joined_df)[14]="Population, low access to store (%), 2015"

colnames(joined_df)[15]="Low income & low access to store, 2015"

colnames(joined_df)[16]="Low income & low access to store (% change), 2010 - 15"

colnames(joined_df)[17]="Low income & low access to store (%), 2015"

colnames(joined_df)[18]="Households, no car & low access to store, 2015"

colnames(joined_df)[19]="Households, no car & low access to store (% change), 2010 - 15"

colnames(joined_df)[20]="Percent Household No Car, Low Access to Store (2015)"

colnames(joined_df)[21]="SNAP households, low access to store, 2015"

colnames(joined_df)[22]="Percent SNAP Household, Low Access to Store (2015)"

colnames(joined_df)[23]="Children, low access to store, 2015"

colnames(joined_df)[24]="Children, Low Access to Store (% Change from 2010-15)"

colnames(joined_df)[25]="Percent Children Low Access to Store (2015)"

colnames(joined_df)[26]="Whites, Low Access to Store (2015)"

colnames(joined_df)[27]="Percent Whites, Low Access to Store (2015)"

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