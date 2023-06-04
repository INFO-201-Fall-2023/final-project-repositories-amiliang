library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)

joined_df <- read.csv("join.csv")

grouped <- group_by(joined_df, State)

#Now Here's the Code for the UI:

state_controls <-  sidebarPanel(
  h1("Choose a State!"),
  selectInput(
  print(grouped),
    inputId = "state_name",
    label = "State Abbreviation", 
  choices = sort(grouped$State[grouped$State != "DC"])),
  p("The top graph displays data for your chosen state, while the botton graph covers the entire U.S. 
    See how your state stacks up! If a graph is blank, that means we totally lack data for that state. If a graph seems suspiciously empty, that means many of its counties lack data. Curse you, NA values!"),
  br(),
  p("Hint: Mississippi, Oklahoma, and Texas have particularly striking correlations for the free lunch scatterplot, while Alabama and Florida show
  strong correlations for the racial demographics scatterplot."))


main_pg_controls <- sidebarPanel(
  h1("What State Did You Grow Up In?"),
  selectInput(
    inputId = "state_nm",
    label = "Select a State", 
    choices = sort(grouped$State[grouped$State != "DC"])
  ),
  uiOutput(outputId = "state_info")
)
#add header with info! then the control panel will be a vertical bar


main_pg <- tabPanel("Intro",
                    titlePanel("Examining Food Access for Public School Students:"),
                    # h1("About Us"),
                    h3("What Data from the U.S. Department of Agriculture and the National Center for Education Statistics Reveal"),
                    br(),
                     p(HTML("We all know the importance of nutritious food to our health and longevity. Yet access to 
                     grocery stores differs profoundly across the country. Factors like race, income, and location all
                     play a role. Many Americans live in food deserts, or areas without easy access to grocery stores. 
                     They may find it more convenient to eat cheap, highly processed fast food, rather than fresh fruits 
                     and vegetables. We also know that our health as children can often influence the trajectory of our lives.
                     Eating and exercise habits instilled in us at a young age can often stick for life. Therefore, it’s critical 
                     to understand how children, in particular, are uniquely affected by food deserts. Data shows that around <a href=\"https://www.cdc.gov/obesity/data/childhood.html#:~:text=For%20children%20and%20adolescents%20aged,to%2019%2Dyear%2Dolds.\">20%</a> of 
                     American children are obese, and that childhood obesity has gotten worse over the past three decades. If we wish 
                     to combat this worrying trend, we need to understand its root causes, including food access. With a better understanding 
                     of the intersection of geography and food access, we can generate solutions tailored to each region of the country.")),
                    br(), 
                    p(HTML("We chose the following two datasets for our analysis. <a href=\"https://catalog.data.gov/dataset/food-environment-atlas/resource/5c59cd7d-5860-47cd-9d64-5ec02a87bb94\">The first</a>, a Food Environment Atlas collected by the Department of Agriculture, covers access to grocery 
                    stores in every county between 2010 and 2015. <a href=\"https://catalog.data.gov/dataset/public-school-characteristics-2020-21/resource/5826d879-9d90-411f-9072-1761324e982d\">The second</a>, compiled by the National Center for Education Statistics, looks at 
                    demographics of public schools for every county between 2020-21. Variable decoding guide  <a href=\"https://nces.ed.gov/opengis/rest/services/K12_School_Locations/EDGE_ADMINDATA_PUBLICSCH_1920/MapServer/0\">here</a>.")),
                    tags$style(HTML("
            h2 {
            background-color: #aebbff;
            color: Black;
            }")),
                    main_pg_controls, 
                   imageOutput("protest_image"),
                   p(HTML("This is a photo taken of a <a href=\"https://www.amny.com/lifestyle/city-living/he-war-against-food-deserts-children-march-over-brooklyn-bridge-calling-for-healthier-options/\">June 2021 march</a> in Brooklyn organized by the non-profit 
                     Seeds in the Middle. The group calls for more food access for kids in the wake of the 
                     pandemic, especially kids of color. Given that obesity, diabetes, and heart disease are 
                     all risk factors for COVID-19, a lack of food access may have exacerbated the pandemic. 
                     Seeds in the Middle advocates for government-funded fresh food stands in Brooklyn, as well 
                     as free coupons for produce.")),
                   br(),
                   p(HTML("According to our dataset, <a href=\"https://docs.google.com/spreadsheets/d/1iz5fTQMDjJwpeXema_OxoiRYq2upkcsJ0Nj9sHXKCoc/edit?usp=sharing\">Kings County</a>
                   (which contains Brooklyn) did not meet the definition of a 
                   food desert in 2015, as only 0.02% of the population lacked access to grocery stores. However, it did 
                   have a higher-than-average child poverty rate of 32%, with 74% of students qualifying for free lunch. 
                   These numbers indicate that families in Brooklyn may still struggle to pay for healthy meals, as the June 
                   2021 protesters can attest to.")))


takeaway_pg <- tabPanel("Takeaways",
              titlePanel("Major Takeaways"),
              br(),
              imageOutput("lunch_tray"),
              br(),
              p(HTML("In the past few years, the subject of food access for kids has received renewed attention, not least 
              due to Michelle Obama’s <a href=\"https://letsmove.obamawhitehouse.archives.gov/\">Let's Move!</a> initiative. Our study indicates that kids in rural counties experience particular deprivation. For instance, most counties 
                where 100% of the population lacks access to stores are rural. Child poverty rates are also higher, sometimes 
                climbing up to 60%. Accordingly, rural counties receive a disproportionate amount of Title 1 funds.")),
            br(),
            p(HTML("School lunch programs are a different story. Rural counties seem to be on par with cities in terms of their free lunch programs, 
            while suburbs have lower percentages and towns have higher percentages of participation. Nonetheless, we also see a worrying number of rural 
            counties with low access to food <em>and</em> low participation in free lunch programs.")),
          br(),
          p("Compared to other county types, rural counties made less progress in the years of 2010-2015, in terms of improving food access. 
          Indeed, whether a county was rural or urban was a far better predictor of whether it made progress than whether it was a food desert to begin with."),
          br(),
          p("These findings have profound implications for students. The patchwork of benefits for disadvantaged areas, such as Title 1 and free lunch programs, 
          varies greatly from county to county. For educators and researchers alike, the question remains: How can we ensure every child has access to the healthy food they need to grow and thrive?"))

lunch_plot_view <-  tabPanel("Free Lunch Scatterplot", 
                             fluidRow(
                              column(width = 9,
                             plotOutput(outputId = "state_lunch_scatter", width = "100%", height = "300px"),
                             plotOutput(outputId = "US_lunch_scatter", width = "100%", height = "300px"),
                              ),
                             br(),
                             p(HTML("Free lunch programs are critical in filling gaps in food access. During the pandemic, 
                             Congress passed a <a href=\"https://spanberger.house.gov/posts/spanberger-introduces-bipartisan-bill-to-extend-increased-reimbursements-for-school-meal-programs-help-schools-feed-students\">bill</a> to provide lunch free of charge to all students. An estimated 10 million students benefited.
                             It was set to lapse in June 2022, but was extended through the 2022-2023 school year.")),
                            p("But now that it's almost summer, the future of the program is up in the air, especially given that 
                              the House of Representatives has since switched from Democratic to Republican control.")),
                          br(),
                          p(HTML("Interestingly, this graph suggests that free lunch programs aren’t necessarily utilized by food deserts. Counties 
                            are all over the map in terms of free lunch, but generally, under 50% of residents lack access to stores. Distressingly, 
                            though, many rural counties are in the lower right quadrant with low store access <em>and</em> skimpy free lunch programs.")))

racial_demo_view <- tabPanel("Racial Demographics Scatterplot", 
                             plotOutput(outputId = "demo_scatter"),
                             plotOutput(outputId = "US_demo_scatter"),
                             br(),
                             p("Based on this chart, it seems that a significant number of rural counties with a higher percent of non-Hispanic white students 
                               also have low access to stores. One limitation of our data, though, is that it doesn’t include the category of Latino/a."),
                             br(),
                             p(HTML("Although our data doesn’t show a strong correlation between these two variables, outside research suggests that racial disparities have 
                                    contributed to food deserts in minority communities, including in <a href=\"https://www.seattle.gov/rsji/racial-equity-research/food-insecurity\">Seattle.</a>")))

change_barchart_view <- tabPanel("Rural vs. Urban Barchart",
                                 plotOutput(outputId = "change_barchart"),
                                 br(),
                                 p(HTML("As our data spans the years 2010-2015, we wanted to ask a simple question: 
                                 “Did food access improve, deteriorate, or plateau in that period?” In 2010, 
                                 Obama signed into law the <a href=\"https://www.usda.gov/media/press-releases/2014/05/20/fact-sheet-healthy-hunger-free-kids-act-school-meals-implementation\">Healthy, Hunger-Free Kids Act</a>, which tightened school lunch 
                                 standards and increased funding for free lunch programs. Although it’s tough to gauge 
                                 the effect of a single law, we wanted to see whether the overall climate has improved.
                                We split up counties into two types: those whose food access increased (represented by “Positive”), 
                                and those whose access decreased (represented by “Negative”). As this graph reveals, urban counties 
                                made substantially more progress than rural counties. Around ⅔ of urban counties made progress, but 
                                only half of rural counties did.")))

poverty_barchart_view <- tabPanel("Child Poverty Barchart",
                                  plotOutput(outputId = "poverty_barchart"),
                                  br(),
                                  p(HTML("To examine whether inequality between counties is worsening, we split up counties by another dimension: 
                                  child poverty. In 2015, the average child poverty rate, across counties, was 23%. (The average rate for the 
                                  country was <a href=\"https://www.childrensdefense.org/wp-content/uploads/2018/08/child-poverty-in-america-2015.pdf\">20%</a>, indicating that high-poverty counties may have been artificially inflating the average.) 
                                  Happily, more people lived in counties below the average than above the average. But it’s not all good news: 
                                  more high-poverty counties saw a worsening of store access, even though the overall trend is heartening.
                                  Use the slider to investigate how different states compare.")),
                                  
                   sliderInput(inputId = "slider2", label = h3("States Within This Range of Child Poverty"), min = 0, max = 100, value = c(40, 60)),
                   uiOutput(outputId = "filtered_states"))

fd_barchart_view <- tabPanel("Food Desert Barchart",
                             plotOutput(outputId = "fd_barchart"),
                             br(),
                             p(HTML("This chart examines inequality from another angle: food deserts. Per the <a href=\"https://www.ers.usda.gov/data-products/food-access-research-atlas/documentation/#:~:text=Definition%3A%20A%20low%2Dincome%20tract,supercenter%2C%20or%20large%20grocery%20store.\">USDA's definition,</a> a county is a food desert 
                             if more than 33% of residents lack access to grocery stores. After classifying counties in our dataset, it’s apparent 
                             that in 2015, more Americans lived in non-food-desert than food desert areas. It seems that the two county types made 
                             equivalent progress. This is a finding that warrants more investigation: why are geography and child poverty so much better 
                             predictors of the direction of food access? After all, one might expect current food access to predict future food access!")))
                             

title_1_barchart_view <- tabPanel("Title 1 Barchart",
                       plotOutput(outputId = "title_1_barchart"),
                       br(),
                       p("Here, we zero in on two extremes from the last graph: counties where either 100% of schools are eligible for Title 1 (the right bar) or 
                       where only a small percentage of schools are eligible (the left bar). We put counties into the latter bucket if their funding percentage 
                       was less than half the national average of 80%. It’s important to remember, though, that many schools were marked “NA” for Title 1 eligibility. 
                       Indeed, missing data is probably why our average of 80% is so high."),
                       br(),
                       p("Slightly more students, it turns out, go to school in counties with a dearth of funding, rather than an abundance of funding. While rural 
                         counties are overrepresented in the 100% category, suburbs and cities dominate the small-percentage category."))

county_type_violin_view <- tabPanel("County Type Violin", 
                        plotOutput(outputId = "county_violin"),
                        br(),
                        p("This chart suggests that rates of child poverty were significantly higher in rural and town counties than in suburban and city counties in 2015. 
                        Suburbs seem to have the lowest rates overall. Each violin has an approximately normal distribution, with values clustered around 20-30%."))

title_1_violin_view <- tabPanel("Title 1 Violin",
                        plotOutput(outputId = "title_1_violin"),
                        br(),
                        p(HTML("When the Title 1 benefit for disadvantaged school districts was established in 1965, eligibility was based on one formula. 
                        Now, four formulas determine funding. Each year, Congress shifts Title 1 funds by weighting some elements of this equation more 
                        heavily than others. As the <a href=\"https://www.brookings.edu/2023/02/01/a-deep-dive-on-how-title-i-funds-are-allocated/\">Brookings Insitution points out,</a> Title 1 status is an imperfect proxy for poverty. Yet it has 
                        tangible ramifications, including for free lunch programs, plus COVID-19 relief funds. It’s critical to know where—and to whom—these funds are going.")),
                        br(),
                        p("Whereas the last graph had reasonably bell-shaped (if lumpy) distributions, this one’s are more peculiar. 50%+ rates of Title 1 eligibility are most 
                        common, with rural counties receiving disproportionate funds."))


factors_main_view <- tabPanel("About", 
         h3("About the Data"),
         p(HTML("Honestly, we were surprized by some of these results. We included a \"gut check\" button so you can test your hypotheses against the data, too.")),
         br(),
         p(HTML("Note: most free lunches are paid for by the federal government through the <a href=\"https://www.fns.usda.gov/nslp\">National School Lunch Program</a>, 
        although some states provide additional funding.")))

change_pg <- tabPanel("Change Over Time",
                      fluidPage(
                      titlePanel("Change from 2010-2015"),
                      
                      mainPanel(
                        tabsetPanel(
                          change_barchart_view,
                          poverty_barchart_view,
                          fd_barchart_view
                        )
                      )
                    )
                  )
                      

factors_pg <- tabPanel("Factors",
                       fluidPage(
                         titlePanel("Across the 50 States"),
                         
                         sidebarLayout(
                           state_controls,
                           
                           mainPanel(
                             
                             tabsetPanel(
                               radioButtons(
                                            inputId = "yes_no",
                                            label = h3("Gut Check: Are These Results What You Expect to See?"),
                                            choices = list("Yes" = "Yes", "No" = "No", "Maaaaybe?" = "Maybe?"),
                                            selected = "Yes"
                               )),
                             
                             tabsetPanel(
                               factors_main_view,
                               lunch_plot_view,
                               racial_demo_view
                               )
                             )
                           )
                         )
                       )


                  
contrasts_pg <- tabPanel("Contrasts",
                         fluidPage(
                           titlePanel("Contrasts"),
                           mainPanel(
                             tabsetPanel(
                             county_type_violin_view,
                             title_1_violin_view,
                             title_1_barchart_view
                             )
                           )
                         )  
)

ui <- navbarPage("INFO 201 Final Project",
                 main_pg,
                 factors_pg,
                 contrasts_pg,
                 change_pg,
                 takeaway_pg)


# Define server logic
server <- function(input, output) {
  
  get_stats <- function(df, state_nm) {
    state_nm <- as.character(state_nm)
    state_df <- filter(df, State == state_nm)
    avg_lunch <- mean(state_df$percent_free_lunch, na.rm = TRUE)
    avg_lunch <- round(avg_lunch, 0)
    num_student <- sum(state_df$num_student, na.rm = TRUE)
    perc_charter <- mean(state_df$Percent_Charter_or_Magnet, na.rm = TRUE)
    perc_charter <- round(perc_charter, 0)
    perc_white <- mean(state_df$perc_non_hispanic_white_students, na.rm = TRUE)
    perc_white <- round(perc_white, 0)
  stats <- paste("Total number of students:", num_student, br(), "Percent of students who receive free lunch:", avg_lunch, "%", br(), "Percent of non-Hispanic white students:", perc_white, "%", br(), "Percent of schools that are charters or magnets:", br(), perc_charter, "%")
    return(HTML(stats))
  }
  
  output$state_info <- renderUI({
    stats <- get_stats(joined_df, input$state_nm)
    return(stats)
  })
  
  output$filtered_states <- renderUI({
    grouped <- group_by(joined_df, State)
    grouped <- filter(grouped, !is.na(CHILDPOVRATE15))
    grouped <- summarize(grouped, avg_child_pov = mean(as.numeric(CHILDPOVRATE15), na.rm = TRUE))
    filtered_states <- filter(grouped, avg_child_pov >= input$slider2[1] & avg_child_pov <= input$slider2[2])
    filtered_states$State <- unique(filtered_states$State)
    state_list <- paste(filtered_states$State, collapse = ", ")  
    state_text <- paste("States:", state_list)  
    return(tags$h4(state_text))
  })
  
  output$protest_image <- renderImage({
    list(src = "Fresh_Food_Protest.jpg",
         width = 520,
         height = 370,
        deleteFile = FALSE)
  })
  
  output$lunch_tray <- renderImage({
    list(src = "lunch_tray.jpg",
         width = 580,
         height = 387,
         deleteFile = FALSE)
  })
  
  output$title_1_violin <- renderPlot({
    county_type_order <- c("Rural", "Town", "Suburb", "City")
    joined_df$County_Type <- factor(joined_df$County_Type, levels = county_type_order)
    title_1_violin <- ggplot(data = joined_df, aes(x = County_Type, y = Percent_Title_1, fill = County_Type)) +
    geom_violin(aes(fill = County_Type)) +
    labs(y = "Percent of Schools Eligible for Title 1", x = "County Type", fill = "County Type")
    return(title_1_violin)
  })
  
  
  output$county_violin <- renderPlot({
    county_type_order <- c("Rural", "Town", "Suburb", "City")
    joined_df$County_Type <- factor(joined_df$County_Type, levels = county_type_order)
    county_violin <- ggplot(data = joined_df, aes(x = County_Type, y = CHILDPOVRATE15, fill = County_Type)) +
      geom_violin(aes(fill = County_Type)) +
      labs(y = "Child Poverty Rate (2015)", x = "County Type", fill = "County Type") 
    return(county_violin)
  })
  
  output$title_1_barchart <- renderPlot({
    title_1_order <-  c("Low_Title_1_Funding", "100%_Title_1_Eligible")
    filtered_title_1 <- filter(joined_df, !is.na(relative_title_1))
    filtered_title_1$relative_title_1 <- factor(filtered_title_1$relative_title_1, levels = title_1_order)
    filtered_title_1$County_Type <- factor(filtered_title_1$County_Type, levels = c("Rural", "Town", "Suburb", "City"))
    title_1_barchart <- ggplot(data = filtered_title_1, aes(fill = County_Type, x = relative_title_1, y = num_student)) +
      geom_bar(position = "stack", stat = "identity", show.legend = TRUE) +
      labs(x = "Amount of Title 1 Funding", fill = "County Type", y = "Number of Students")
    return(title_1_barchart)
  })
  
  
  output$fd_barchart <- renderPlot({
    bar <- ggplot(data = joined_df, aes(fill = as.factor(pop_food_access_direction_of_change), x = food_desert, y = Population_Estimate_2015)) +
      geom_bar(position="stack", stat="identity") +
      ggtitle("Comparing Rates of Change Against Food Desert Status") +
      labs(x = "Whether This County Qualifies as a Food Desert", y = "County Population (2015)", fill = "Direction of Change in Store Access (2010-2015)") +
      scale_fill_manual(values = c("Positive" = "lightgreen", "Negative" = "indianred3", "No_Change" = "blue", "No_Value_Given" = "gray"),
                        labels = c("Negative" = "Negative", "Positive" = "Positive", "No_Change" = "No Change", "No_Value_Given" = "No Value Given"))
    return(bar)
  })
  
  
  output$change_barchart <- renderPlot({
    bar <- ggplot(data = joined_df, aes(fill = as.factor(pop_food_access_direction_of_change), x = rural_urban, y = Population_Estimate_2015)) +
      geom_bar(position="stack", stat="identity") +
      ggtitle("Comparing Rates of Change Across County Types") +
      labs(x = "County Type", y = "County Population (2015)", fill = "Direction of Change in Store Access (2010-2015)") +
      scale_fill_manual(values = c("Positive" = "lightgreen", "Negative" = "indianred3", "No_Change" = "blue", "No_Value_Given" = "gray"),
                        labels = c("Negative" = "Negative", "Positive" = "Positive", "No_Change" = "No Change", "No_Value_Given" = "No Value Given"))
    return(bar)
  })
  
  output$poverty_barchart <- renderPlot({
    poverty_barchart <- ggplot(data = joined_df, aes(fill = as.factor(pop_food_access_direction_of_change), x = above_avg_child_pov, y = Population_Estimate_2015)) +
    geom_bar(position="stack", stat="identity") +
    ggtitle("Comparing Rates of Change Against Child Poverty") +
    labs(x = "Whether Child Poverty Rate is Above Average", y = "County Population (2015)", fill = "Direction of Change in Store Access (2010-2015)") +
    scale_fill_manual(values = c("Positive" = "lightgreen", "Negative" = "indianred3", "No_Change" = "blue", "No_Value_Given" = "gray"),
    labels = c("Negative" = "Negative", "Positive" = "Positive", "No_Change" = "No Change", "No_Value_Given" = "No Value Given"))
   return(poverty_barchart)
    })
  
  output$state_lunch_scatter <- renderPlot({
    state_info <- filter(grouped, State == input$state_name)
    s <- ggplot(data = state_info, aes(y = percent_free_lunch, x = Population..low.access.to.store......2015, size = num_student, color = County)) +
      geom_point(aes(col = County_Type)) +
      labs(y = "% Free Lunch Students", x = "% Residents w/ Low Access to Store", size = "Total Students") +
      ggtitle("Percent Free Lunch vs Store Access in", paste(input$state_name)) 
    return(s)
  })
  
  output$US_lunch_scatter <- renderPlot({
    lunch <- ggplot(data = joined_df, aes(y = percent_free_lunch, x = Population..low.access.to.store......2015, size = num_student, color = County_Type)) +
      geom_point() +
      scale_size_continuous(range = c(0.5, 9)) +
      labs(y = "% Free Lunch Students", x = "% Residents w/ Low Access to Store", size = "Number of Students", color = "County Type") +
      ggtitle("Percent Free Lunch vs Store Access in the U.S.") 
    return(lunch)
  })
  
  output$US_demo_scatter <- renderPlot({
    US_scatter <- ggplot(data = joined_df, aes(x = perc_non_hispanic_white_students, y = Population..low.access.to.store......2015, size = num_student, color = County)) +
      geom_point(aes(col=County_Type)) + ggtitle("Racial Demographics vs Grocery Store Access") +
      labs(x = "% Non-Hispanic White Students (2020)", y = "% Residents with Low Access to Stores (2015)", size = "Number of Students") 
    return(US_scatter)
  })
  
  output$scatter <- renderPlot({
    state_info <- filter(grouped, State == input$state_name)
    state_info <- filter(state_info, !is.na(Percent_Charter_or_Magnet))
    state_info <- filter(state_info, !is.na(Percent_Title_1))
    s <- ggplot(data = state_info, aes(x = percent_free_lunch, y = Percent.Children.Low.Access.to.Store..2015., size = num_student, color = County)) +
      geom_point(aes(col = County_Type)) +
      labs(x = "% Free Lunch Students", y = "% Children w/ Low Access to Store", size = "Number of Students") +
      ggtitle("Percent Free Lunch vs Store Access for Kids ", paste("Percent Free Lunch vs Store Access for Kids", input$state_name))
    return(s)
  })
  
  output$demo_scatter <- renderPlot({
    state_info <- filter(grouped, State == input$state_name)
    d <- ggplot(data = state_info, aes(x = perc_non_hispanic_white_students, y = Population..low.access.to.store......2015, size = num_student, color = County)) +
      geom_point(aes(col=County_Type)) + ggtitle("Racial Demographics vs Grocery Store Access") +
      labs(x = "% Non-Hispanic White Students (2020)", y = "% Residents w/ Low Access to Stores (2015)", size = "Number of Students") 
    return(d)
  })
  
  output$title_1_violin <- renderPlot({
    county_type_order <- c("Rural", "Town", "Suburb", "City")
    joined_df$County_Type <- factor(joined_df$County_Type, levels = county_type_order)
    title_1_violin <- ggplot(data = joined_df, aes(x = County_Type, y = Percent_Title_1, fill = County_Type)) +
    geom_violin(aes(fill = County_Type)) +
    labs(y = "Percent of Schools Eligible for Title 1", x = "County Type", fill = "County Type")
    return(title_1_violin)
  })
  
  
  output$county_violin <- renderPlot({
    county_type_order <- c("Rural", "Town", "Suburb", "City")
    joined_df$County_Type <- factor(joined_df$County_Type, levels = county_type_order)
    county_violin <- ggplot(data = joined_df, aes(x = County_Type, y = CHILDPOVRATE15, fill = County_Type)) +
      geom_violin(aes(fill = County_Type)) +
      labs(y = "Child Poverty Rate (2015)", x = "County Type", fill = "County Type") 
    return(county_violin)
  })
  
  output$fd_barchart <- renderPlot({
    bar <- ggplot(data = joined_df, aes(fill = as.factor(pop_food_access_direction_of_change), x = food_desert, y = Population_Estimate_2015)) +
      geom_bar(position="stack", stat="identity") +
      ggtitle("Comparing Rates of Change Across County Types") +
      labs(x = "Whether This County Qualifies as a Food Desert", y = "County Population (2015)", fill = "Direction of Change in Store Access (2010-2015)") +
      scale_fill_manual(values = c("Positive" = "lightgreen", "Negative" = "indianred3", "No_Change" = "blue", "No_Value_Given" = "gray"),
                        labels = c("Negative" = "Negative", "Positive" = "Positive", "No_Change" = "No Change", "No_Value_Given" = "No Value Given"))
    return(bar)
  })
  
  output$change_barchart <- renderPlot({
    bar <- ggplot(data = joined_df, aes(fill = as.factor(pop_food_access_direction_of_change), x = rural_urban, y = Population_Estimate_2015)) +
      geom_bar(position="stack", stat="identity") +
      ggtitle("Comparing Rates of Change Across County Types") +
      labs(x = "County Type", y = "County Population (2015)", fill = "Direction of Change in Store Access (2010-2015)") +
      scale_fill_manual(values = c("Positive" = "lightgreen", "Negative" = "indianred3", "No_Change" = "blue", "No_Value_Given" = "gray"),
                        labels = c("Negative" = "Negative", "Positive" = "Positive", "No_Change" = "No Change", "No_Value_Given" = "No Value Given"))
    return(bar)
  })
  
  output$poverty_barchart <- renderPlot({
    barchart <- ggplot(data = joined_df, aes(fill = as.factor(pop_food_access_direction_of_change), x = above_avg_child_pov, y = Population_Estimate_2015)) +
    geom_bar(position="stack", stat="identity") +
    ggtitle("Comparing Rates of Change Against Child Poverty") +
    labs(x = "Whether Child Poverty Rate is Above Average", y = "County Population (2015)", fill = "Direction of Change in Store Access (2010-2015)") +
    scale_fill_manual(values = c("Positive" = "lightgreen", "Negative" = "indianred3", "No_Change" = "blue", "No_Value_Given" = "gray"),
    labels = c("Negative" = "Negative", "Positive" = "Positive", "No_Change" = "No Change", "No_Value_Given" = "No Value Given"))
   return(barchart)
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
