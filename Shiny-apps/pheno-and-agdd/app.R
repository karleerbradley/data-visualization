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
library(plotly)


# Define UI for application 
ui <- 

  fluidPage(

    # Application title
    titlePanel(h3(span("Mapping Heat Accumulation and Plant Phenology Data", style = "color:darkblue")), div(style="padding: 0px 0px; margin:0%")),hr(),

    # top panel 
    
    # the text at the top of the app that tells user what to do
    fluidRow(div(style="height:.2px "),
        column(12,div(style = "padding: 0px 0px; margin-top:-1em"),
          helpText(h6(span("Select a NEON terrestrial site and choose at least one year. Then select if you want to observe phenology data, AGDDs, or both for the selected site and years.",
                                  style = "color:darkblue"))))
    ),
    fluidRow(div(style = "margin:-1200px"),
         column(3, style = "margin-bottom:-20px",
          # choose which site you want to observe
          pickerInput("select", label = h5(span("NEON Sites", style = "color:darkblue")), 
                      choices = list("Harvard Forest (HARV)" = "HARV", "Bartlett Experimental Forest (BART)" = "BART", 
                                "Smithsonian Environmental Research Center (SERC)" = "SERC", "University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", 
                                 "The Universtiy of Kansas Field Station (UKFS)" = "UKFS", "Oak Ridge (ORNL)" = "ORNL",
                                 "LBJ National Grassland (CLBJ)" = "CLBJ", "Abby Road (ABBY)" = "ABBY", "Toolik Lake (TOOL)" = "TOOL",
                                 "Caribou Creek (BONA)" = "BONA"), selected = "HARV")
          ),

        column(4,style="margin-right:-12px; margin-bottom:-20px",

         # can choose if you want to look at phenophases, AGDDs, or both using checkbox buttons
         prettyRadioButtons("option", label = h5(span("Choose what to observe", style = "color:darkblue")), choices = c("Phenophases" ="phenos","AGDDs"="agdd", "Phenophases & AGDDs"="both"),
                             selected = "phenos", status = "danger", outline = TRUE, inline = TRUE)
        ),
          column(4, style="margin-left:-30px; margin-bottom:-20px",
          # checkbox so can pick as many years that are available for that site as you want
          prettyCheckboxGroup("checkGroup", label = h5(span("Years", style = "color:darkblue")), 
                             choices = list("2015"="2015","2016"="2016", "2017" = "2017", "2018"="2018", "2019"="2019"), 
                             selected = NULL, status = "danger", outline = TRUE, inline = TRUE)
          )
          ,
        column(1, style="margin-left:-50px; margin-top:15px; margin-bottom:-20px",
         
         # button to plot that you have to click again to refresh the graph
          actionButton("goplot", label = span("PLOT", style = "color:darkblue")))
          
        ), hr(),
        

  # depending on what the user chooses to observe, the following plots are made
        mainPanel(
          conditionalPanel(condition = "input.option == 'agdd'", plotlyOutput("yearsPlot", width = "150%", height = "500px")),
          conditionalPanel(condition = "input.option == 'phenos'", plotlyOutput("phenoPlot", width = "150%", height = "500px")),
          conditionalPanel(condition = "input.option == 'both'", plotOutput("bothPlot", width = "150%", height = "500px"))
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


  checkedGroups <- eventReactive(input$goplot, {
    yr <- input$checkGroup
    sort(yr)})
  
  

##### if the user chooses to observe agdds, then this plot is generated
 # eventReactive(input$goplot, {
 output$yearsPlot <- renderPlotly({
  # if they haven't chosen a year, this appears
   validate(
     need(input$checkGroup != "", "Please select at least one year to plot.")
   )

  
   withProgress(message = "Making plot", value = 0,{
     siteO <- input$select
     yr <- input$checkGroup
     sort(yr)
     
     #eventReactive(input$goplot,{
     
     # loading the data from the API
     dpid2 <- as.character('DP1.00002.001') 
     agddLoad <- loadByProduct(dpID=dpid2, site=siteO, package="basic", check.size = FALSE, 
                               startdate = c(paste(checkedGroups()[1],"-01", sep = "")), enddate = c(paste(tail(checkedGroups(), n=1), "-12", sep = "")), avg = "30")
  
     SAAT <- agddLoad$SAAT_30min
     df <- SAAT
     
     # cleaning the data:
     
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
       arrange(date) %>% # had to add this so that the dates were in order
       select(siteID, date, verticalPosition, minTempF, maxTempF) %>%
       mutate(dayMax=max(maxTempF), dayMin=min(minTempF)) %>% 
       select(siteID, date, dayMax, dayMin)%>%
       distinct() # only keeps one row for each date with the max, min, and mean
     
     # calculating the mean for max and mins of each day
     years_data$dayMean <- (years_data$dayMax + years_data$dayMin)/2
     
     
     #caluculate daily GDD
     years_data$GDD <- ifelse(years_data$dayMean-50 < 0, 0, round(years_data$dayMean-50, 0))
     
     # adding useful columns to dataframe for date analysis
     years_data$dayOfYear <- yday(years_data$date)
     years_data$year <- substr(years_data$date, 1, 4)
     years_data$monthDay <- format.Date(years_data$date, format="%B %d")
     
     # starts AGDD over at 0 at the beginning of a new year
     require(data.table)
     years_data <- data.table(years_data)
     years_data[, AGDD := cumsum(GDD), by=list(year, siteID)]
     
     # filtering data based on user selection
     site_select <- years_data %>%
       filter(siteID %in% siteO) %>%
       filter(year %in% yr)
     

  # font stuff for legend and axes
     l <- list(
       font = list(family = "'Helvetica Neue',Helvetica,Arial,sans-serif",
         size = 12,
         color = "#000"),
       bordercolor = "#FFFFFF",
       borderwidth = 2)
     
     f <- list(
       font = list(size = 15, family = "'Helvetica Neue',Helvetica,Arial,sans-serif")
     )
     
     ff <- list(size=10)
     
     x <- list(
       title = "Day of Year",
       titlefont = f, tickfont=ff
     )
     y <- list(
       title = "AGDDs",
       titlefont = f, tickfont=ff)
     

      # plots the data with the different years as the different lines
  # observeEvent(
     p <- ggplot(data=site_select, aes(x=dayOfYear, y=AGDD, color = as.factor(year),group = 1,
                                       text = paste("</br>Date:",monthDay, "</br>AGDDs:",AGDD, "</br>Year:", year))) +
        geom_path() + xlab("Day of Year") + ggtitle("Accumulated Growing Degree Days Across Years") +
        scale_color_brewer(palette = "Dark2") + ylab("AGGDs")  + xlim(c(0,366))+ theme_bw() +
        theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20, hjust = 0.5),
              axis.title = element_text(lineheight = .8, size = 10),
              #legend.title = element_text(lineheight = .8, size = 10, vjust = 0.5),
              legend.text = element_text(size = 5), 
              axis.text = element_text(size = 5)) + labs(color="")
     
     # this part allows user to hover over graph and see different values
     ggplotly(p, tooltip = c("text")) %>% config(displayModeBar = FALSE) %>% 
       layout(legend=list(x=100,y=.5, text = "Year", yanchor="top"), xaxis=x, yaxis=y) %>% layout(legend = l) %>% 
       add_annotations(text="Year", xref="paper", yref="paper",x=1.02, xanchor="left",y=0.5, yanchor="bottom",legendtitle=TRUE, showarrow=FALSE) 
}) 
  
   })
 
# })
   
 
 
 
 
 
    
# if the user chooses to observe phenophases, then this plot is generated
 output$phenoPlot <- renderPlotly({
  # prints this is user hasn't chosen a year
    validate(
      need(input$checkGroup != "", "Please select at least one year to plot.")
    )
   
   siteO <- input$select
   yr <- input$checkGroup
   sort(yr)
   
   # loading data from API
   dpid <- as.character('DP1.10055.001')
   
  # observeEvent(input$goplot,{
   phenoLoad <- loadByProduct(dpID=dpid, site=siteO, package="basic", check.size = FALSE, 
                              startdate = c(paste(checkedGroups()[1],"-01", sep = "")), enddate = c(paste(tail(checkedGroups(), n=1), "-12", sep = "")))
   
   # cleaning the data:
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
   
  
   # filter the data based on user input
   pheno <- phe_ind %>%
     filter(growthForm=="Deciduous broadleaf")
   pheno <- pheno %>%
     filter(siteID %in% input$select) %>%
     filter(year %in% input$checkGroup)
   
   # adding common names
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
   
   
   # only look at the yes's
   pheno <- filter(pheno, phenophaseStatus %in% "yes")
   
   # counting total individuals by day for each commonName
   total <- pheno %>%
     group_by(commonName) %>%
     count(date)
   # renaming counted column to "total"
   names(total)[which(names(total) == "n")] <- "total"
   # joining total dateframe to df
   pheno <- left_join(pheno, total, by = c("date", "commonName"))
   
   # counting total individuals by day for each phenophase for each commonName
   phenos <- pheno %>%
     group_by(commonName, phenophaseName) %>%
     count(date)
   names(phenos)[which(names(phenos)=="n")] <- "number"
   pheno <- left_join(pheno, phenos, by = c("date", "commonName", "phenophaseName"))
   
   # calculating percentage by dividing count of each phenophase by total count
   pheno$percent <- ((pheno$number)/pheno$total)*100
   

   
   pheno$dayOfYear <- yday(pheno$date)
   pheno$year <- substr(pheno$date, 1, 4)
   pheno$monthDay <- format.Date(pheno$date, format="%B %d")
   pheno$phenophaseName <- factor(pheno$phenophaseName, levels = c("Leaves", "Falling leaves", "Colored leaves",  "Increasing leaf size","Breaking leaf buds",  "Open flowers"))
   
  # legend font stuff
   l <- list(
     font = list(family = "'Helvetica Neue',Helvetica,Arial,sans-serif",
                 size = 12,
                 color = "#000"),
     bordercolor = "#FFFFFF",
     borderwidth = 2, orientation = "h")

   #observeEvent(
   # plot the graph 
   p <- ggplot(pheno, aes(x = dayOfYear, y = percent, fill = phenophaseName, color = phenophaseName,group = 1,
                                text = paste("</br>Date:",monthDay, "</br>Percent:",percent, "</br>Phenophase:", phenophaseName))) +
      #geom_density(alpha=0.3,stat = "identity", position = "stack") +
     geom_density(alpha=0.3,stat = "identity", position = position_dodge(width = .5)) +
      theme_bw() + facet_grid(cols = vars(commonName),rows = vars(year), scale = "free_y") +
      xlab("Day Of Year") + ylab("Percentage of Individuals\nin Each Phenophase") + xlim(0,366) +
      ggtitle("Plant Phenophase Density for Selected Site") +
      theme(plot.title = element_text(lineheight = 1, face = "bold", size = 20, hjust = 0.5),
            axis.title = element_text(lineheight = .8, size = 15),
            #legend.title = element_text(lineheight = .8, size = 20),
            legend.text = element_text(size = 15), 
            axis.text = element_text(size = 10),
            strip.text.x = element_text(size = 10) , strip.text.y = element_text(size = 10)) +
      scale_color_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1") +
      theme(legend.position = "bottom") #+ labs(fill = "Phenophase", color = "Phenophase")
   
   # hover over plot
   gp <- ggplotly(p, tooltip = c("text")) %>% config(displayModeBar = FALSE)  %>% 
     layout(legend=list(x=.25,y=-.4, text = "Phenophase", yanchor= "middle")) %>% layout(legend = l) %>% 
     add_annotations(text="Phenophase", xref="paper", yref="paper",x=.5, xanchor="top",y=-.3, yanchor="bottom",legendtitle=TRUE, showarrow=FALSE) 
   
   # changing position of x and y axis titles
   # x was -.04
   gp[['x']][['layout']][['annotations']][[2]][['x']] <- -0.02
   gp %>% layout(margin = list(l = 75))
   
   gp[['x']][['layout']][['annotations']][[1]][['y']] <- -0.05
   gp %>% layout(margin = list(l = 75)) %>% layout(margin = list(t = 75))
   
 })
 #})
    
 
 
 
 
 
 
 
# if the user chooses to observe both phenophases and agdds, then this plot is generated 
    output$bothPlot <- renderPlot({
      # prints until user selects a year
      validate(
        need(input$checkGroup != "", "Please select at least one year to plot.")
      )
      
      #eventReactive(input$goplot,{
      
      siteO <- input$select
      yr <- input$checkGroup
      sort(yr)
      
      # loading phenology data from API
      dpid <- as.character('DP1.10055.001')
      phenoLoad <- loadByProduct(dpID=dpid, site=siteO, package="basic", check.size = FALSE, 
                                 startdate = c(paste(checkedGroups()[1],"-01", sep = "")), enddate = c(paste(tail(checkedGroups(), n=1), "-12", sep = "")))
      
      # cleaning phenology data:
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
      
      # filtering pheno data based on user input
      pheno <- phe_ind %>%
        filter(growthForm=="Deciduous broadleaf")
      pheno <- pheno %>%
        filter(siteID %in% input$select) %>%
        filter(year %in% input$checkGroup)
      
      # adding common names
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
      
      
      # only look at the yes's
      pheno <- filter(pheno, phenophaseStatus %in% "yes")
      
      # counting total individuals by day for each commonName
      total <- pheno %>%
        group_by(commonName) %>%
        count(date)
      # renaming counted column to "total"
      names(total)[which(names(total) == "n")] <- "total"
      # joining total dateframe to df
      pheno <- left_join(pheno, total, by = c("date", "commonName"))
      
      # counting total individuals by day for each phenophase for each commonName
      phenos <- pheno %>%
        group_by(commonName, phenophaseName) %>%
        count(date)
      names(phenos)[which(names(phenos)=="n")] <- "number"
      pheno <- left_join(pheno, phenos, by = c("date", "commonName", "phenophaseName"))
      
      # calculating percentage by dividing count of each phenophase by total count
      pheno$percent <- ((pheno$number)/pheno$total)*100
      

      pheno$dayOfYear <- yday(pheno$date)
      pheno$year <- substr(pheno$date, 1, 4)
      pheno$monthDay <- format.Date(pheno$date, format="%B %d")
      pheno$phenophaseName <- factor(pheno$phenophaseName, levels = c("Leaves", "Falling leaves", "Colored leaves",  "Increasing leaf size","Breaking leaf buds",  "Open flowers"))
      
      # loading the air temp data
      dpid2 <- as.character('DP1.00002.001') 
      agddLoad <- loadByProduct(dpID=dpid2, site=siteO, package="basic", check.size = FALSE, 
                                startdate = c(paste(checkedGroups()[1],"-01", sep = "")), enddate = c(paste(tail(checkedGroups(), n=1), "-12", sep = "")), avg = "30")
      SAAT <- agddLoad$SAAT_30min
      df <- SAAT
      
      # cleaning air temp data:
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
      years_data$GDD <- ifelse(years_data$dayMean-50 < 0, 0, round(years_data$dayMean-50, 0))
      
      # adding useful columns to dataframe for date analysis
      years_data$dayOfYear <- yday(years_data$date)
      years_data$year <- substr(years_data$date, 1, 4)
      years_data$monthDay <- format.Date(years_data$date, format="%B %d")
      
      # starts AGDD over at 0 at the beginning of a new year
      require(data.table)
      years_data <- data.table(years_data)
      years_data[, AGDD := cumsum(GDD), by=list(year, siteID)]
      
      
      
      # filtering the data based on user input
      site_select <- years_data %>%
        filter(siteID %in% siteO) %>%
        filter(year %in% yr)
      
      
      ay <- list(overlaying = "y", side = "right", title = "AGDDs", autotick = TRUE, showlines=FALSE)
      
      # legend font stuff
      l <- list(
        font = list(family = "'Helvetica Neue',Helvetica,Arial,sans-serif",
                    size = 12,
                    color = "#000"),
        bordercolor = "#FFFFFF",
        borderwidth = 2, orientation = "h")
      
      
     # observeEvent(
      # plot
      # can hover on solo graphs but not this one
      ggplot(data=pheno, mapping=aes(x = dayOfYear, y = percent, fill = phenophaseName, color = phenophaseName
                                               # ,group =1,
                                               # text = paste("</br>Date:",monthDay, "</br>Percent:",percent, "</br>Phenophase:", phenophaseName)
                                           )) +
        geom_density(alpha=0.3,stat = "identity", position = position_dodge(width = 0.5)) +
        theme_bw() + facet_grid(cols = vars(commonName),rows = vars(year), scale = "free_y") +
        scale_color_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1") +
        theme(legend.position = "bottom") + labs(fill = "Phenophase", color = "Phenophase") + 
        geom_path(data=site_select,aes(x=dayOfYear, y=AGDD/65, 
                                       #, group=1, text = paste("</br>Date:", monthDay, "</br>AGDDs:", AGDD)
                                       ), inherit.aes = FALSE, size = 1.5, color = "blue")+
        scale_y_continuous(sec.axis = sec_axis(~.*65, name = "AGDDs")) +
        labs(x = "Day of Year", y = "Percentage of Individuals\nin Each Phenophase") + ggtitle("DBL Phenophases and AGDDs") +
        theme(plot.title = element_text(lineheight = 1, face = "bold", size = 20, hjust = 0.5),
              axis.title = element_text(lineheight = .8, size = 15),
              legend.title = element_text(lineheight = .8, size = 20),
              legend.text = element_text(size = 15),
              axis.text = element_text(size = 10),
              strip.text.x = element_text(size = 10), strip.text.y = element_text(size = 10),
              axis.title.y.right = element_text(color = "blue"), 
              axis.text.y.right = element_text(color = "blue"))
      
      
      # ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE) %>% 
      #   layout(legend=list(x=.25,y=-.4, text = "Phenophase", yanchor="left")) %>% layout(legend = l) %>% 
      #   add_annotations(text="Phenophase", xref="paper", yref="paper",x=.5, xanchor="top",y=-.3, yanchor="bottom",legendtitle=TRUE, showarrow=FALSE) %>%
      #   add_lines(data = site_select, x=~dayOfYear, y=~AGDD, yaxis="y2", showlegend =FALSE, inherit = FALSE) %>% 
      #   layout(yaxis2=ay)
      

 
      
  #  }) 
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
