# pheno-tabs

library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)

# load the data
DBL <- read.csv("DBL-tabs.csv", stringsAsFactors = FALSE)


# Define UI for application that draws a histogram
ui <- navbarPage(
  
  strong(span("Phenophases", style = "color:darkblue")),
  
  tabPanel(span("Phenophases By Year", style = "color:darkblue"),
           
           sidebarLayout(
             sidebarPanel(
               helpText("Choose a site to observe the percentages of individuals in each phenophase across the years chosen."),
               # dropdown select menu
               selectInput("select", label = h3(span("NEON Sites", style = "color:darkblue")), choices = list("Harvard Forest (HARV)" = "HARV", "Bartlett Experimental Forest (BART)" = "BART", 
                                "Smithsonian Environmental Research Center (SERC)" = "SERC", "University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", 
                                "The Universtiy of Kansas Field Station (UKFS)" = "UKFS", "Oak Ridge (ORNL)" = "ORNL",
                                     "LBJ National Grassland (CLBJ)" = "CLBJ", "Abby Road (ABBY)" = "ABBY", "Toolik Lake (TOOL)" = "TOOL",
                                     "Caribou Creek (BONA)" = "BONA"), selected = "HARV"),
               # checkbox so can pick as many years as you want
               checkboxGroupInput("checkGroup", label = h3(span("Years", style = "color:darkblue")), 
                                  choices = list("2015"="2015","2016"="2016", "2017" = "2017", "2018"="2018", "2019"="2019"), selected = NULL)
               
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("areaPlot")
             )
           )
  ),
  
  tabPanel(span("Phenophases Across Sites", style = "color:darkblue"),
           sidebarLayout(
             sidebarPanel(
               helpText("Choose a year and a phenophase to observe in that year and choose which sites to compare in the chosen phenophase."),
               # drop down menu to pick one year to look at 
               selectInput("select2", label = h3(span("Year", style = "color:darkblue")), choices = list("2014"="2014", "2015"="2015", "2016"="2016",
                                                                                                        "2017"="2017", "2018"="2018", "2019"="2019"), selected = NULL),
               # drop down to pick one phenophase to observe
               selectInput("select3", label = h3(span("Phenophase", style = "color:darkblue")), choices = list("Breaking Leaf Buds"="Breaking leaf buds",
                                                                                                               "Increasing Leaf Size"="Increasing leaf size", "Open Flowers"="Open flowers",
                                                                                                               "Leaves"="Leaves", "Colored Leaves"="Colored leaves", 
                                                                                                               "Falling Leaves"="Falling leaves"), selected = NULL) ,
               # can choose multiple sites to plot
               checkboxGroupInput("checkGroup2", label = h3(span("NEON Sites", style = "color:darkblue")),
                                  choices = list("Harvard Forest (HARV)" = "HARV", "Bartlett Experimental Forest (BART)" = "BART",
                                                 "Smithsonian Environmental Research Center (SERC)" = "SERC", "University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", 
                                                 "The Universtiy of Kansas Field Station (UKFS)" = "UKFS", "Oak Ridge (ORNL)" = "ORNL",
                                                 "LBJ National Grassland (CLBJ)" = "CLBJ", "Abby Road (ABBY)" = "ABBY", "Toolik Lake (TOOL)" = "TOOL",
                                                 "Caribou Creek (BONA)" = "BONA"), selected = NULL)
               
             ),
             
             # Show a plot
             mainPanel(
               plotOutput("phenoPlot")
             )
           )

           )

  )



# Define server logic required to draw a histogram
server <- function(input, output, session) {

  observe({
    x <- input$select
    
    if (x == "ORNL" | x == "BART" | x=="HARV" | x=="UNDE")
      updateCheckboxGroupInput(session, "checkGroup", choices = list("2015"="2015",
                                                                     "2016"="2016", "2017" = "2017", "2018"="2018", "2019"="2019"))
    if (x == "SERC" | x=="UKFS" | x=="ABBY")
      updateCheckboxGroupInput(session, "checkGroup", choices = list("2016"="2016", "2017" = "2017", "2018"="2018", "2019"="2019"))
    if (x=="CLBJ" | x=="TOOL")
      updateCheckboxGroupInput(session, "checkGroup", choices = list("2017" = "2017", "2018"="2018", "2019"="2019"))
    if (x=="BONA")
      updateCheckboxGroupInput(session, "checkGroup", choices = list( "2018"="2018", "2019"="2019"))
    
  })
  
  
  # when a year isn't chosen, an error shows in the main panel
  # with this, instead of putting the error message, it shows this sentence so user
  # knows what they need to do.
  output$areaPlot <- renderPlot({
    validate(
      need(input$checkGroup != "", "Please select at least one year to plot.")
    )
    
    pheno <- DBL %>%
      filter(siteID %in% input$select) %>%
      filter(year %in% input$checkGroup)
    
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
      #geom_density(alpha=0.3,stat = "identity", position = position_dodge(width = .1)) +
      geom_density(alpha=0.3,stat = "identity", position = "stack") +  
      theme_bw() + facet_grid(cols = vars(commonName),rows = vars(year), scale = "free_y") +
      xlab("Day Of Year") + ylab("% of Individuals") + xlim(0,366) +
      ggtitle("Phenophase Density for Selected Site") +
      theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20, hjust = 0.5)) +
      theme(text = element_text(size = 15)) +
      scale_color_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1") +
      theme(legend.position = "bottom") + labs(fill = "Phenophase", color = "Phenophase")
    
    
    
  })
  
  observe({
    # the year chosen
    y <- input$select2
    # the phenophase(s) chosen
    z <- input$select3
    
    # the following code is changing the select options depending on what there is data for
    # some sites don't have data for all the years between 2014 and 2019
    # if a site does have data for a particular year, it doesn't mean they have data for a particular phenophase
    # I changed these options so that you aren't given the option to choose something that won't be plotted
    if (y == "2014"){
      updateCheckboxGroupInput(session, "checkGroup2", choices = list("Harvard Forest (HARV)" = "HARV",
                                                                     "Bartlett Experimental Forest (BART)" = "BART","University of Notre Dame Environmental Research Center (UNDE)" = "UNDE"))
      
      if (z == "Breaking leaf buds" | z == "Open flowers" | z == "Increasing leaf size"){
        updateCheckboxGroupInput(session, "checkGroup2", choices = list("Harvard Forest (HARV)" = "HARV",
                                                                       "Bartlett Experimental Forest (BART)" = "BART"))
      }
    }
    
    if (y == "2015"){
      updateCheckboxGroupInput(session, "checkGroup2", choices = list("Harvard Forest (HARV)" = "HARV",
                                                                     "Bartlett Experimental Forest (BART)" = "BART","University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", "Oak Ridge (ORNL)" = "ORNL"))
      
    }
    if (y == "2016"){
      updateCheckboxGroupInput(session, "checkGroup2", choices = list("Harvard Forest (HARV)" = "HARV",
                                                                     "Bartlett Experimental Forest (BART)" = "BART","University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", "Oak Ridge (ORNL)" = "ORNL",
                                                                     "Smithsonian Environmental Research Center (SERC)" = "SERC","Abby Road (ABBY)" = "ABBY",
                                                                     "The Universtiy of Kansas Field Station (UKFS)" = "UKFS"))
      
    }
    if (y == "2017"){
      updateCheckboxGroupInput(session, "checkGroup2", choices = list("Harvard Forest (HARV)" = "HARV",
                                                                     "Bartlett Experimental Forest (BART)" = "BART","University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", "Oak Ridge (ORNL)" = "ORNL",
                                                                     "Smithsonian Environmental Research Center (SERC)" = "SERC","Abby Road (ABBY)" = "ABBY",
                                                                     "The Universtiy of Kansas Field Station (UKFS)" = "UKFS","LBJ National Grassland (CLBJ)" = "CLBJ",
                                                                     "Toolik Lake (TOOL)" = "TOOL", "Caribou Creek (BONA)" = "BONA"))
      
      if (z == "Breaking leaf buds" | z == "Open flowers"){
        updateCheckboxGroupInput(session, "checkGroup2", choices = list("Harvard Forest (HARV)" = "HARV",
                                                                       "Bartlett Experimental Forest (BART)" = "BART","University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", "Oak Ridge (ORNL)" = "ORNL",
                                                                       "Smithsonian Environmental Research Center (SERC)" = "SERC","Abby Road (ABBY)" = "ABBY",
                                                                       "The Universtiy of Kansas Field Station (UKFS)" = "UKFS", "LBJ National Grassland (CLBJ)" = "CLBJ"))
      }
      if (z == "Increasing leaf size"){
        updateCheckboxGroupInput(session, "checkGroup2", choices = list("Harvard Forest (HARV)" = "HARV",
                                                                       "Bartlett Experimental Forest (BART)" = "BART","University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", "Oak Ridge (ORNL)" = "ORNL",
                                                                       "Smithsonian Environmental Research Center (SERC)" = "SERC","Abby Road (ABBY)" = "ABBY",
                                                                       "The Universtiy of Kansas Field Station (UKFS)" = "UKFS", "LBJ National Grassland (CLBJ)" = "CLBJ",
                                                                       "Caribou Creek (BONA)" = "BONA"))
      }
    }
    if (y == "2018" ){
      updateCheckboxGroupInput(session, "checkGroup2", choices = list("Harvard Forest (HARV)" = "HARV",
                                                                     "Bartlett Experimental Forest (BART)" = "BART","University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", "Oak Ridge (ORNL)" = "ORNL",
                                                                     "Smithsonian Environmental Research Center (SERC)" = "SERC","Abby Road (ABBY)" = "ABBY",
                                                                     "The Universtiy of Kansas Field Station (UKFS)" = "UKFS","LBJ National Grassland (CLBJ)" = "CLBJ",
                                                                     "Toolik Lake (TOOL)" = "TOOL", "Caribou Creek (BONA)" = "BONA"))
      
      if (z == "Breaking leaf buds"){
        updateCheckboxGroupInput(session, "checkGroup2", choices = list("Harvard Forest (HARV)" = "HARV",
                                                                       "Bartlett Experimental Forest (BART)" = "BART","University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", "Oak Ridge (ORNL)" = "ORNL",
                                                                       "Smithsonian Environmental Research Center (SERC)" = "SERC","Abby Road (ABBY)" = "ABBY",
                                                                       "The Universtiy of Kansas Field Station (UKFS)" = "UKFS","LBJ National Grassland (CLBJ)" = "CLBJ",
                                                                       "Toolik Lake (TOOL)" = "TOOL"))
      }
      if (z == "Increasing leaf size"){
        updateCheckboxGroupInput(session, "checkGroup2", choices = list("Harvard Forest (HARV)" = "HARV",
                                                                       "Bartlett Experimental Forest (BART)" = "BART","University of Notre Dame Environmental Research Center (UNDE)" = "UNDE",
                                                                       "Smithsonian Environmental Research Center (SERC)" = "SERC","Abby Road (ABBY)" = "ABBY",
                                                                       "The Universtiy of Kansas Field Station (UKFS)" = "UKFS","LBJ National Grassland (CLBJ)" = "CLBJ",
                                                                       "Toolik Lake (TOOL)" = "TOOL", "Caribou Creek (BONA)" = "BONA"))
      }
      if (z == "Open flowers"){
        updateCheckboxGroupInput(session, "checkGroup2", choices = list("Harvard Forest (HARV)" = "HARV",
                                                                       "Bartlett Experimental Forest (BART)" = "BART","University of Notre Dame Environmental Research Center (UNDE)" = "UNDE",
                                                                       "Smithsonian Environmental Research Center (SERC)" = "SERC","Abby Road (ABBY)" = "ABBY",
                                                                       "The Universtiy of Kansas Field Station (UKFS)" = "UKFS","LBJ National Grassland (CLBJ)" = "CLBJ",
                                                                       "Toolik Lake (TOOL)" = "TOOL"))
      }
    }
    if (y == "2019"){
      updateCheckboxGroupInput(session, "checkGroup2", choices = list("Harvard Forest (HARV)" = "HARV",
                                                                     "Bartlett Experimental Forest (BART)" = "BART","University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", "Oak Ridge (ORNL)" = "ORNL",
                                                                     "Smithsonian Environmental Research Center (SERC)" = "SERC","Abby Road (ABBY)" = "ABBY",
                                                                     "The Universtiy of Kansas Field Station (UKFS)" = "UKFS","LBJ National Grassland (CLBJ)" = "CLBJ",
                                                                     "Toolik Lake (TOOL)" = "TOOL", "Caribou Creek (BONA)" = "BONA"))
      
      if (z == "Breaking leaf buds"){
        updateCheckboxGroupInput(session, "checkGroup2", choices = list("Harvard Forest (HARV)" = "HARV",
                                                                       "University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", "Oak Ridge (ORNL)" = "ORNL",
                                                                       "Smithsonian Environmental Research Center (SERC)" = "SERC","Abby Road (ABBY)" = "ABBY",
                                                                       "The Universtiy of Kansas Field Station (UKFS)" = "UKFS", "Caribou Creek (BONA)" = "BONA"))
      }
      if (z == "Increasing leaf size" | z == "Leaves"){
        updateCheckboxGroupInput(session, "checkGroup2", choices = list("Harvard Forest (HARV)" = "HARV",
                                                                       "Oak Ridge (ORNL)" = "ORNL", "Smithsonian Environmental Research Center (SERC)" = "SERC",
                                                                       "Abby Road (ABBY)" = "ABBY", "The Universtiy of Kansas Field Station (UKFS)" = "UKFS"))
      }
      if (z == "Open flowers"){
        updateCheckboxGroupInput(session, "checkGroup2", choices = list("Harvard Forest (HARV)" = "HARV",
                                                                       "Bartlett Experimental Forest (BART)" = "BART","University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", "Oak Ridge (ORNL)" = "ORNL",
                                                                       "Smithsonian Environmental Research Center (SERC)" = "SERC","Abby Road (ABBY)" = "ABBY",
                                                                       "The Universtiy of Kansas Field Station (UKFS)" = "UKFS"))
      }
      if (z == "Colored leaves" | z == "Falling leaves")
        updateCheckboxGroupInput(session, "checkGroup2", choices = character(0) )
    }
  })
  
  # code for the plot
  output$phenoPlot <- renderPlot({
    
    # filtering the data depending on what was chosen
    phenoSelect <- DBL %>%
      filter(year %in% input$select2) %>%
      filter(phenophaseName %in% input$select3) %>%
      filter(siteID %in% input$checkGroup2)
    
    # look at the total individuals in phenophase status by day
    phenoSamp <- phenoSelect %>%
      group_by(siteID) %>%
      count(date)
    phenoStat <- phenoSelect %>%
      group_by(date, siteID) %>%
      count(phenophaseStatus) 
    phenoStat <- full_join(phenoSamp, phenoStat, by = c("date", "siteID"))
    ungroup(phenoStat)
    
    # only look at the yes's
    phenoStat_T <- filter(phenoStat, phenophaseStatus %in% "yes")
    
    # plot the percentage of individuals in the leaves phenophase
    # convert to percentage
    phenoStat_T$percent <- ((phenoStat_T$n.y)/phenoStat_T$n.x)*100
    
    phenoStat_T$dayOfYear <- yday(phenoStat_T$date)
    
    ggplot(phenoStat_T, aes(dayOfYear, percent, color = siteID)) +
      geom_line(stat = "identity", na.rm = TRUE) +
      ggtitle("Percentage in Chosen Phenophase") +
      xlab("Day Of Year") + ylab("% of Individuals") + xlim(0,366) + ylim(0,100) + theme_bw() +
      theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20, hjust = 0.5)) +
      theme(text = element_text(size = 15)) +
      scale_colour_manual(values = c("maroon1", "purple", "orange", "navyblue", "cyan",
                                     "brown", "darkcyan", "black", "springgreen1", "red"))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
