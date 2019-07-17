# pheno-and-agdd
# meadowlark-style with agdd on top
# tried to make buttons that would plot only phenophases, only agdds, but not working with if statements
# how to make the top panel shorter so that the graph can take up most of the room??

library(shiny)
library(shinyWidgets)
library(htmltools)
library(ggplot2)
library(dplyr)
library(lubridate)
library(neonUtilities)
library(geoNEON)

# load the data, both use the same data
# DBL <- read.csv("area.csv", stringsAsFactors = FALSE)
# temp_data <- read.csv("years.csv", stringsAsFactors = FALSE)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(h3(span("Mapping Heat Accumulation and Phenology Data", style = "color:darkblue")), div(style="padding: 0px 0px; margin:0%")),hr(),

    # Sidebar 
    fluidRow(div(style="height:.2px "),
        column(12,div(style = "padding: 0px 0px; margin-top:-1em"),
          helpText(h6(span("Select a NEON terrestrial site and choose at least one year. Then select if you want to observe phenology data, AGDDs, or both for the selected site and years.",
                                  style = "color:darkblue"))))
    ),
    fluidRow(
         column(4, style="height:75px",
          # choose which site you want to observe
          pickerInput("select", label = h5(span("NEON Sites", style = "color:darkblue")), 
                      choices = list("Harvard Forest (HARV)" = "HARV", "Bartlett Experimental Forest (BART)" = "BART", 
                                "Smithsonian Environmental Research Center (SERC)" = "SERC", "University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", 
                                 "The Universtiy of Kansas Field Station (UKFS)" = "UKFS", "Oak Ridge (ORNL)" = "ORNL",
                                 "LBJ National Grassland (CLBJ)" = "CLBJ", "Abby Road (ABBY)" = "ABBY", "Toolik Lake (TOOL)" = "TOOL",
                                 "Caribou Creek (BONA)" = "BONA"), selected = "HARV")
          ),

        column(4,style="height:75px",

         # can choose if you want to look at phenophases, AGDDs, or both using checkbox buttons
         prettyRadioButtons("option", label = h5(span("Choose what to observe", style = "color:darkblue")), choices = c("Phenophases" ="phenos","AGDDs"="agdd", "Phenophases & AGDDs"="both"),
                             selected = "phenos", status = "danger", outline = TRUE, inline = TRUE)
        ),
          column(4, style="height:75px",
          # checkbox so can pick as many years that are available for that site as you want
          prettyCheckboxGroup("checkGroup", label = h5(span("Years", style = "color:darkblue")), 
                             choices = list("2015"="2015","2016"="2016", "2017" = "2017", "2018"="2018", "2019"="2019"), 
                             selected = NULL, status = "danger", outline = TRUE, inline = TRUE))
          
          #,br(),
         # button to plot that you have to click again to refresh the graph
          #actionButton("goplot", label = "PLOT")
          
        ), hr(),
        

  # depending on what the user chooses to observe, the following plots are made
        mainPanel(
           #plotOutput("dataPlot")
          conditionalPanel(condition = "input.option == 'agdd'", plotOutput("yearsPlot", width = "150%", height = "500px")),
          conditionalPanel(condition = "input.option == 'phenos'", plotOutput("phenoPlot", width = "150%", height = "500px")),
          conditionalPanel(condition = "input.option == 'both'", plotOutput("bothPlot", width = "150%", height = "500px"))
          # maybe this is how you plot if wanted to use checkbox buttons instead of radio buttons for option
          # conditionalPanel(condition = "input.option == 'agdd' & input.option=='phenos'", plotOutput("bothPlot"))
        )
    )


# Define server logic
server <- function(input, output, session) {
  


  # updating the choices based on years avaiable for each site.
  observe({
    x <- input$select
    if (x == "ORNL" | x == "BART" | x=="HARV" | x=="UNDE")
      updatePrettyCheckboxGroup(session, "checkGroup", choices = list("2015"="2015",
                                                                     "2016"="2016", "2017" = "2017", "2018"="2018", "2019"="2019"), prettyOptions = list(status = "danger", outline = TRUE), inline = TRUE)
    if (x == "SERC" | x=="UKFS" | x=="ABBY")
      updatePrettyCheckboxGroup(session, "checkGroup", choices = list("2016"="2016", "2017" = "2017", "2018"="2018", "2019"="2019"), prettyOptions = list(status = "danger", outline = TRUE), inline = TRUE)
    if (x=="CLBJ" | x=="TOOL")
      updatePrettyCheckboxGroup(session, "checkGroup", choices = list("2017" = "2017", "2018"="2018"), prettyOptions = list(status = "danger", outline = TRUE), inline = TRUE)
    if (x=="BONA")
      updatePrettyCheckboxGroup(session, "checkGroup", choices = list( "2018"="2018", "2019"="2019"), prettyOptions = list(status = "danger", outline = TRUE), inline = TRUE)
  })

  
  
 # if the user chooses to observe agdds, then this plot is generated
 output$yearsPlot <- renderPlot({
   
   validate(
     need(input$checkGroup != "", "Please select at least one year to plot.")
   )
   
   siteO <- input$select
   yr <- input$checkGroup
   sort(yr)

   dpid2 <- as.character('DP1.00002.001') 
   agddLoad <- loadByProduct(dpID=dpid2, site=siteO, package="basic", check.size = FALSE, 
                             startdate = c(paste(yr[1],"-01", sep = "")), enddate = c(paste(tail(yr, n=1), "-12", sep = "")), avg = "30")
   
   
   SAAT <- agddLoad$SAAT_30min
   df <- SAAT
   
   # define function to convert temp c to f 
   c_to_f <- function(x)  (x * 1.8 + 32)
   
   df_2 <- filter(SAAT, !is.na(tempSingleMinimum), !is.na(tempSingleMaximum))
   
   # convert df min temps and max temps from c to f
   df_2$minTempF <- c_to_f(df_2$tempSingleMinimum)
   df_2$maxTempF <- c_to_f(df_2$tempSingleMaximum)
   
   #pull date value from dateTime
   # pulls the first 10 indices from endDateTime (yyyy-mm-dd) and creates a new column with just the date
   df_2$date <- substr(df_2$endDateTime, 1, 10)
   
   
   years_data <- df_2%>%
     group_by(date, siteID)%>%
     #filter(year(date)=="2017") %>% # added the year here so that AGDDs starts from beginning of year
     arrange(date) %>% # had to add this so that the dates were in order
     select(siteID, date, verticalPosition, minTempF, maxTempF) %>%
     mutate(dayMax=max(maxTempF), dayMin=min(minTempF)) %>% 
     select(siteID, date, dayMax, dayMin)%>%
     distinct() # only keeps one row for each date with the max, min, and mean
   
   
   # calculating the mean for max and mins of each day
   years_data$dayMean <- (years_data$dayMax + years_data$dayMin)/2
   
   
   #caluculate daily GDD
   # base temp is 50F
   # if mean2-50<0, then the number of GDDs is 0
   # if mean2-50>0, then it rounds the value to the nearest whole number, and that's the number of GDDs for that date
   years_data$GDD <- ifelse(years_data$dayMean-50 < 0, 0, round(years_data$dayMean-50, 0))
   
   # adding useful columns to dataframe for date analysis
   years_data$dayOfYear <- yday(years_data$date)
   years_data$year <- substr(years_data$date, 1, 4)
   
   # new code that starts AGDD over at 0 at the beginning of a new year
   require(data.table)
   years_data <- data.table(years_data)
   years_data[, AGDD := cumsum(GDD), by=list(year, siteID)]
   
   
   site_select <- years_data %>%
     filter(siteID %in% siteO) %>%
     filter(year %in% yr)
   

      # plots the data with the different years as the different lines
      ggplot(data=site_select, aes(x=dayOfYear, y=AGDD, color = as.factor(year))) +
        geom_path() + xlab("Day of Year") + ggtitle("Accumulated Growing Degree Days Across Years") +
        scale_color_brewer(palette = "Dark2") + ylab("AGGDs")  + xlim(c(0,366))+ theme_bw() +
        theme(plot.title = element_text(lineheight = .8, face = "bold", size = 30, hjust = 0.5),
              axis.title = element_text(lineheight = .8, size = 20),
              legend.title = element_text(lineheight = .8, size = 20),
              legend.text = element_text(size = 15), 
              axis.text = element_text(size = 15)) + labs(color="Year")
 })
   
    
# if the user chooses to observe phenophases, then this plot is generated
 output$phenoPlot <- renderPlot({

    validate(
      need(input$checkGroup != "", "Please select at least one year to plot.")
    )

   siteO <- input$select
   yr <- input$checkGroup
   sort(yr)
   
   dpid <- as.character('DP1.10055.001')
   phenoLoad <- loadByProduct(dpID=dpid, site=siteO, package="basic", check.size = FALSE, 
                              startdate = c(paste(yr[1],"-01", sep = "")), enddate = c(paste(tail(yr, n=1), "-12", sep = "")))
   
   ind <- phenoLoad$phe_perindividual
   status <- phenoLoad$phe_statusintensity
   
   # remove the uid columns for both the ind and the status data
   ind <- select(ind, -uid)
   status <- select(status, -uid)
   
   # remove duplicate rows in the ind data and the status data
   ind_noD <- distinct(ind)
   status_noD <- distinct(status)
   
   # rename variables in the status object to have "Stat" at the end of the variable name before joining the datasets
   status_noD <- rename(status_noD, editedDateStat = editedDate, measuredByStat = measuredBy, recordedByStat = recordedBy,
                        samplingProtocolVersionStat = samplingProtocolVersion, remarksStat = remarks, dataQFStat = dataQF)
   
   # rename variables in ind so that it doesn't conflict later on when joining the dataframes
   ind_noD <- rename(ind_noD, addDate = date)
   
   # convert the date columns from character class to date class for ind and status
   ind_noD$editedDate <- as.Date(ind_noD$editedDate)
   status_noD$date <- as.Date(status_noD$date)
   
   # retain only the latest editedDate for each individualID on ind and get rid of duplicate dates
   ind_lastnoD <- ind_noD %>%
     group_by(individualID) %>%
     filter(editedDate == max(editedDate)) %>%
     group_by(editedDate, individualID) %>%
     filter(row_number() == 1)
   
   # join the two dataframes together into one table
   # this will be the table that is used to then narrow sites and species and phenophases
   phe_ind <- left_join(status_noD, ind_lastnoD)
   
   # removing columns with mostly NAs that aren't needed
   phe_ind <- select(phe_ind, -dayOfYear, -samplingProtocolVersionStat, -remarksStat, -dataQFStat)
   phe_ind <- select(phe_ind, -geodeticDatum, -coordinateUncertainty, -elevationUncertainty, -elevationUncertainty, -sampleLatitude, 
                     -sampleLongitude, -sampleCoordinateUncertainty, -sampleElevation, -sampleElevationUncertainty, 
                     -identificationQualifier, -vstTag, -dataQF)
   # removing other columns that don't seem to be relevant
   phe_ind <- select(phe_ind, -measuredByStat, -recordedByStat, -sampleGeodeticDatum, -samplingProtocolVersion,
                     -measuredBy, -identifiedBy, -recordedBy)
   
   # adding other date columns for better date analysis
   phe_ind$year <- substr(phe_ind$date, 1, 4)
   
   # using geoNEON package to find locations
   # adding latitude and longitude from geoNEON to phe_ind, and elevation
   spatialOnly <- def.extr.geo.os(phe_ind, 'namedLocation', locOnly = T)
   phe_ind$latitude <- spatialOnly$api.decimalLatitude[match(phe_ind$namedLocation, spatialOnly$data.locationName)]
   phe_ind$longitude <- spatialOnly$api.decimalLongitude[match(phe_ind$namedLocation, spatialOnly$data.locationName)]
   phe_ind$elevation <- spatialOnly$api.elevation[match(phe_ind$namedLocation, spatialOnly$data.locationName)]
   
   
   pheno <- phe_ind %>%
     filter(growthForm=="Deciduous broadleaf")
   pheno <- pheno %>%
     filter(siteID %in% input$select) %>%
     filter(year %in% input$checkGroup)
   
   pheno <- mutate(pheno, commonName = ifelse(taxonID %in% "ACRU", "Red Maple", 
                                       ifelse(taxonID %in% "QURU", "Northern Red Oak", 
                                       ifelse(taxonID %in% "ACPE", "Snakebark Maple", 
                                       ifelse(taxonID %in% "FAGR", "American Beech",
                                       ifelse(taxonID %in% "LITU", "Tulip Tree", 
                                       ifelse(taxonID %in% "LIST2", "Sweetgum",
                                       ifelse(taxonID %in% "LIBE3", "Northern Spicebush",
                                       ifelse(taxonID %in% "POTR5", "Quaking Aspen",
                                       ifelse(taxonID %in% "ACSA3", "Sugar Maple",
                                       ifelse(taxonID %in% "COCO6", "Beaked Hazelnut",
                                       ifelse(taxonID %in% "SYOR", "Coralberry",
                                       ifelse(taxonID %in% "CAOV2", "Shagbark Hickory",
                                       ifelse(taxonID %in% "CEOC", "Common Hackberry",
                                       ifelse(taxonID %in% "QUMO4", "Chestnut Oak",
                                       ifelse(taxonID %in% "COFL2", "Flowering Dogwood",
                                       ifelse(taxonID %in% "QUMA3", "Blackjack Oak",
                                       ifelse(taxonID %in% "COCOC", "California Hazelnut",
                                       ifelse(taxonID %in% "BEGL", "Resin Birch", "other")))))))))))))))))))
   
   
   # look at the total individuals in phenophase status by day
   phenoSamp <- pheno %>%
     group_by(siteID) %>%
     count(date)
   phenoStat <- pheno %>%
     group_by(date, siteID, taxonID, phenophaseName, commonName) %>%
     count(phenophaseStatus) 
   phenoStat <- full_join(phenoSamp, phenoStat, by = c("date", "siteID"))
   ungroup(phenoStat)
   
   # only look at the yes's
   phenoStat_T <- filter(phenoStat, phenophaseStatus %in% "yes")
   
   # plot the percentage of individuals in the leaves phenophase
   # convert to percentage
   phenoStat_T$percent <- ((phenoStat_T$n.y)/phenoStat_T$n.x)*100
   
   phenoStat_T$dayOfYear <- yday(phenoStat_T$date)
   phenoStat_T$year <- substr(phenoStat_T$date, 1, 4)
   phenoStat_T$phenophaseName <- factor(phenoStat_T$phenophaseName, levels = c("Leaves", "Falling leaves", "Colored leaves",  "Increasing leaf size","Breaking leaf buds",  "Open flowers"))
   

    ggplot(phenoStat_T, aes(x = dayOfYear, y = percent, fill = phenophaseName, color = phenophaseName)) +
      geom_density(alpha=0.3,stat = "identity", position = position_dodge(width = .1)) +
      #geom_density(alpha=0.3,stat = "identity", position = "stack") +
      theme_bw() + facet_grid(cols = vars(commonName),rows = vars(year), scale = "free_y") +
      xlab("Day Of Year") + ylab("% of Individuals") + xlim(0,366) +
      ggtitle("Phenophase Density for Selected Site") +
      theme(plot.title = element_text(lineheight = .8, face = "bold", size = 30, hjust = 0.5),
            axis.title = element_text(lineheight = .8, size = 20),
            legend.title = element_text(lineheight = .8, size = 20),
            legend.text = element_text(size = 15), 
            axis.text = element_text(size = 15), strip.text.x = element_text(size = 15), strip.text.y = element_text(size = 15)) +
      scale_color_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1") +
      theme(legend.position = "bottom") + labs(fill = "Phenophase", color = "Phenophase")

 })
    
 
 
# if the user chooses to observe both phenophases and agdds, then this plot is generated 
    output$bothPlot <- renderPlot({
      
      validate(
        need(input$checkGroup != "", "Please select at least one year to plot.")
      )
      
      siteO <- input$select
      yr <- input$checkGroup
      sort(yr)
      
      dpid <- as.character('DP1.10055.001')
      phenoLoad <- loadByProduct(dpID=dpid, site=siteO, package="basic", check.size = FALSE, 
                                 startdate = c(paste(yr[1],"-01", sep = "")), enddate = c(paste(tail(yr, n=1), "-12", sep = "")))
      
      ind <- phenoLoad$phe_perindividual
      status <- phenoLoad$phe_statusintensity
      
      # remove the uid columns for both the ind and the status data
      ind <- select(ind, -uid)
      status <- select(status, -uid)
      
      # remove duplicate rows in the ind data and the status data
      ind_noD <- distinct(ind)
      status_noD <- distinct(status)
      
      # rename variables in the status object to have "Stat" at the end of the variable name before joining the datasets
      status_noD <- rename(status_noD, editedDateStat = editedDate, measuredByStat = measuredBy, recordedByStat = recordedBy,
                           samplingProtocolVersionStat = samplingProtocolVersion, remarksStat = remarks, dataQFStat = dataQF)
      
      # rename variables in ind so that it doesn't conflict later on when joining the dataframes
      ind_noD <- rename(ind_noD, addDate = date)
      
      # convert the date columns from character class to date class for ind and status
      ind_noD$editedDate <- as.Date(ind_noD$editedDate)
      status_noD$date <- as.Date(status_noD$date)
      
      # retain only the latest editedDate for each individualID on ind and get rid of duplicate dates
      ind_lastnoD <- ind_noD %>%
        group_by(individualID) %>%
        filter(editedDate == max(editedDate)) %>%
        group_by(editedDate, individualID) %>%
        filter(row_number() == 1)
      
      # join the two dataframes together into one table
      # this will be the table that is used to then narrow sites and species and phenophases
      phe_ind <- left_join(status_noD, ind_lastnoD)
      
      # removing columns with mostly NAs that aren't needed
      phe_ind <- select(phe_ind, -dayOfYear, -samplingProtocolVersionStat, -remarksStat, -dataQFStat)
      phe_ind <- select(phe_ind, -geodeticDatum, -coordinateUncertainty, -elevationUncertainty, -elevationUncertainty, -sampleLatitude, 
                        -sampleLongitude, -sampleCoordinateUncertainty, -sampleElevation, -sampleElevationUncertainty, 
                        -identificationQualifier, -vstTag, -dataQF)
      # removing other columns that don't seem to be relevant
      phe_ind <- select(phe_ind, -measuredByStat, -recordedByStat, -sampleGeodeticDatum, -samplingProtocolVersion,
                        -measuredBy, -identifiedBy, -recordedBy)
      
      # adding other date columns for better date analysis
      phe_ind$year <- substr(phe_ind$date, 1, 4)
      
      # using geoNEON package to find locations
      # adding latitude and longitude from geoNEON to phe_ind, and elevation
      spatialOnly <- def.extr.geo.os(phe_ind, 'namedLocation', locOnly = T)
      phe_ind$latitude <- spatialOnly$api.decimalLatitude[match(phe_ind$namedLocation, spatialOnly$data.locationName)]
      phe_ind$longitude <- spatialOnly$api.decimalLongitude[match(phe_ind$namedLocation, spatialOnly$data.locationName)]
      phe_ind$elevation <- spatialOnly$api.elevation[match(phe_ind$namedLocation, spatialOnly$data.locationName)]
      
      
      pheno <- phe_ind %>%
        filter(growthForm=="Deciduous broadleaf")
      pheno <- pheno %>%
        filter(siteID %in% input$select) %>%
        filter(year %in% input$checkGroup)
      
      pheno <- mutate(pheno, commonName = ifelse(taxonID %in% "ACRU", "Red Maple", 
                                          ifelse(taxonID %in% "QURU", "Northern Red Oak", 
                                          ifelse(taxonID %in% "ACPE", "Snakebark Maple", 
                                          ifelse(taxonID %in% "FAGR", "American Beech",
                                          ifelse(taxonID %in% "LITU", "Tulip Tree", 
                                          ifelse(taxonID %in% "LIST2", "Sweetgum",
                                          ifelse(taxonID %in% "LIBE3", "Northern Spicebush",
                                          ifelse(taxonID %in% "POTR5", "Quaking Aspen",
                                          ifelse(taxonID %in% "ACSA3", "Sugar Maple",
                                          ifelse(taxonID %in% "COCO6", "Beaked Hazelnut",
                                          ifelse(taxonID %in% "SYOR", "Coralberry",
                                          ifelse(taxonID %in% "CAOV2", "Shagbark Hickory",
                                          ifelse(taxonID %in% "CEOC", "Common Hackberry",
                                          ifelse(taxonID %in% "QUMO4", "Chestnut Oak",
                                          ifelse(taxonID %in% "COFL2", "Flowering Dogwood",
                                          ifelse(taxonID %in% "QUMA3", "Blackjack Oak",
                                          ifelse(taxonID %in% "COCOC", "California Hazelnut",
                                          ifelse(taxonID %in% "BEGL", "Resin Birch", "other")))))))))))))))))))
      
      
      # look at the total individuals in phenophase status by day
      phenoSamp <- pheno %>%
        group_by(siteID) %>%
        count(date)
      phenoStat <- pheno %>%
        group_by(date, siteID, taxonID, phenophaseName, commonName) %>%
        count(phenophaseStatus) 
      phenoStat <- full_join(phenoSamp, phenoStat, by = c("date", "siteID"))
      ungroup(phenoStat)
      
      # only look at the yes's
      phenoStat_T <- filter(phenoStat, phenophaseStatus %in% "yes")
      
      # plot the percentage of individuals in the leaves phenophase
      # convert to percentage
      phenoStat_T$percent <- ((phenoStat_T$n.y)/phenoStat_T$n.x)*100
      
      phenoStat_T$dayOfYear <- yday(phenoStat_T$date)
      phenoStat_T$year <- substr(phenoStat_T$date, 1, 4)
      phenoStat_T$phenophaseName <- factor(phenoStat_T$phenophaseName, levels = c("Leaves", "Falling leaves", "Colored leaves",  "Increasing leaf size","Breaking leaf buds",  "Open flowers"))
      
      
      dpid2 <- as.character('DP1.00002.001') 
      agddLoad <- loadByProduct(dpID=dpid2, site=siteO, package="basic", check.size = FALSE, 
                                startdate = c(paste(yr[1],"-01", sep = "")), enddate = c(paste(tail(yr, n=1), "-12", sep = "")), avg = "30")
      
      
      SAAT <- agddLoad$SAAT_30min
      df <- SAAT
      
      # define function to convert temp c to f 
      c_to_f <- function(x)  (x * 1.8 + 32)
      
      df_2 <- filter(SAAT, !is.na(tempSingleMinimum), !is.na(tempSingleMaximum))
      
      # convert df min temps and max temps from c to f
      df_2$minTempF <- c_to_f(df_2$tempSingleMinimum)
      df_2$maxTempF <- c_to_f(df_2$tempSingleMaximum)
      
      #pull date value from dateTime
      # pulls the first 10 indices from endDateTime (yyyy-mm-dd) and creates a new column with just the date
      df_2$date <- substr(df_2$endDateTime, 1, 10)
      
      
      years_data <- df_2%>%
        group_by(date, siteID)%>%
        #filter(year(date)=="2017") %>% # added the year here so that AGDDs starts from beginning of year
        arrange(date) %>% # had to add this so that the dates were in order
        select(siteID, date, verticalPosition, minTempF, maxTempF) %>%
        mutate(dayMax=max(maxTempF), dayMin=min(minTempF)) %>% 
        select(siteID, date, dayMax, dayMin)%>%
        distinct() # only keeps one row for each date with the max, min, and mean
      
      
      # calculating the mean for max and mins of each day
      years_data$dayMean <- (years_data$dayMax + years_data$dayMin)/2
      
      
      #caluculate daily GDD
      # base temp is 50F
      # if mean2-50<0, then the number of GDDs is 0
      # if mean2-50>0, then it rounds the value to the nearest whole number, and that's the number of GDDs for that date
      years_data$GDD <- ifelse(years_data$dayMean-50 < 0, 0, round(years_data$dayMean-50, 0))
      
      # adding useful columns to dataframe for date analysis
      years_data$dayOfYear <- yday(years_data$date)
      years_data$year <- substr(years_data$date, 1, 4)
      
      # new code that starts AGDD over at 0 at the beginning of a new year
      require(data.table)
      years_data <- data.table(years_data)
      years_data[, AGDD := cumsum(GDD), by=list(year, siteID)]
      
      
      site_select <- years_data %>%
        filter(siteID %in% siteO) %>%
        filter(year %in% yr)
      
      
      ggplot(data=phenoStat_T, mapping=aes(x = dayOfYear, y = percent, fill = phenophaseName, color = phenophaseName)) +
        geom_density(data=phenoStat_T,alpha=0.3,stat = "identity", position = position_dodge(width = .1)) +
        #geom_density(alpha=0.3,stat = "identity", position = "stack") +  
        theme_bw() + facet_grid(cols = vars(commonName),rows = vars(year), scale = "free_y") +
        scale_color_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1") +
        theme(legend.position = "bottom") + labs(fill = "Phenophase", color = "Phenophase") +
        geom_path(data=site_select,aes(x=dayOfYear, y=AGDD/240), inherit.aes = FALSE, size = 1.5)+
        scale_y_continuous(sec.axis = sec_axis(~.*240, name = "AGDDs")) +
        labs(x = "Day of Year", y = "Percentage of Individuals\nin Each Phenophase") + ggtitle("Phenophases and AGDDs") +
        theme(plot.title = element_text(lineheight = .8, face = "bold", size = 30, hjust = 0.5),
              axis.title = element_text(lineheight = .8, size = 20),
              legend.title = element_text(lineheight = .8, size = 20),
              legend.text = element_text(size = 15), 
              axis.text = element_text(size = 15),
              strip.text.x = element_text(size = 15), strip.text.y = element_text(size = 15))

      
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
