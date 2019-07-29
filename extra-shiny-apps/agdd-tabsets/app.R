# agdd-tabsets
# combines the agdd-path app and the agdd-years app into one agdd app
# each one can be accessed by the tabs on top of the page

library(shiny)
library(dplyr)
library(ggplot2)

# reading the data into app
# this dataframe will be used by both tabs, but it will be filtered in different ways for each of them
temp_data <- read.csv("years_dataTabs.csv", stringsAsFactors = FALSE)


# Define UI for application

# the navbarPage creates tabs on the top
ui <- navbarPage(

    # Application title, this is shown at the very top of the page and is the title for the whole app
   strong(span("Accumulated Growing Degree Days", style = "color:darkblue")),

  
 #  this creates the first tab on the top of the page, which is the agdd-path app 
    tabPanel(span("AGDDs By Year", style = "color:darkblue"),  
      # Sidebar
      # has a checkbox menu for you to choose whichever NEON sites to display data for
      sidebarLayout(
        sidebarPanel(
          helpText(strong(span("Choose a year to observe AGDDs over and choose which sites you want to compare for that year.", style = "color:d"))),
          selectInput("select", label = h3(span("Years", style = "color:darkblue")),
                      choices = list("2015"="2015", "2016"="2016","2017"="2017","2018"="2018", "2019"="2019")),
          # HARV is chosen as a default
          checkboxGroupInput("checkGroup",
                             label = h3(span("NEON Sites", style = "color:darkblue")), 
                             choices = list("Harvard Forest (HARV)" = "HARV", "Bartlett Experimental Forest (BART)" = "BART", "Smithsonian Environmental Research Center (SERC)" = "SERC", 
                                            "University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", "The Universtiy of Kansas Field Station (UKFS)" = "UKFS", "Oak Ridge (ORNL)" = "ORNL",
                                            "LBJ National Grassland (CLBJ)" = "CLBJ", "Abby Road (ABBY)" = "ABBY", "Toolik Lake (TOOL)" = "TOOL",
                                            "Caribou Creek (BONA)" = "BONA"))
        ),
        
        # Show a plot 
        mainPanel(
          plotOutput("pathPlot") 
        )
      )
    ),
    
 # this creates the second tab on the top, which is the agdd-years app
      tabPanel(span("AGDDs Across Years", style = "color:darkblue"),   
               # Sidebar 
               # makes you choose a NEON site with a dropdown menu
               # then lets you choose which years to display for the chosen site
               sidebarLayout(
                 sidebarPanel(
                   helpText("Choose a NEON site to observe AGDDS for and choose which years to compare for that site."),
                   # dropdown select menu
                   selectInput("select2", label = h3(span("NEON Sites", style = "color:darkblue")), choices = list("Harvard Forest (HARV)" = "HARV", "Bartlett Experimental Forest (BART)" = "BART", 
                                                                                  "Smithsonian Environmental Research Center (SERC)" = "SERC", "University of Notre Dame\nEnvironmental Research Center (UNDE)" = "UNDE", 
                                                                                  "The Universtiy of Kansas Field Station (UKFS)" = "UKFS", "Oak Ridge (ORNL)" = "ORNL",
                                                                                  "LBJ National Grassland (CLBJ)" = "CLBJ", "Abby Road (ABBY)" = "ABBY", "Toolik Lake (TOOL)" = "TOOL",
                                                                                  "Caribou Creek (BONA)" = "BONA"), selected = "HARV"),
                   # checkbox so can pick as many years as you want
                   checkboxGroupInput("checkGroup2", label = h3(span("Years", style = "color:darkblue")), choices = list("2014" = "2014", "2015"="2015",
                                                                                        "2016"="2016", "2017" = "2017", "2018"="2018", "2019"="2019"), selected = NULL)
                   
                 ),
                 
                 # shows the plot in the main panel
                 mainPanel(
                   plotOutput("pathPlot2")
                 )
               )
      
)
)


# Define server logic 
server <- function(input, output, session) {
  
# this first observe and output is for the first tab
  observe({
    x <- input$select
    
    if (x == "2015")
      updateCheckboxGroupInput(session, "checkGroup", choices = list("Harvard Forest (HARV)" = "HARV", 
                                                                     "Bartlett Experimental Forest (BART)" = "BART",
                                                                     "University of Notre Dame Environmental Research Center (UNDE)" = "UNDE",
                                                                     "Oak Ridge (ORNL)" = "ORNL") )
    if (x=="2016")
      updateCheckboxGroupInput(session, "checkGroup", choices = list("Harvard Forest (HARV)" = "HARV", 
                                                                     "Bartlett Experimental Forest (BART)" = "BART", "Smithsonian Environmental Research Center (SERC)" = "SERC", 
                                                                     "University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", 
                                                                     "The Universtiy of Kansas Field Station (UKFS)" = "UKFS", "Oak Ridge (ORNL)" = "ORNL",
                                                                     "Abby Road (ABBY)" = "ABBY"))
    if (x == "2017")
      updateCheckboxGroupInput(session, "checkGroup", choices = list("Harvard Forest (HARV)" = "HARV", 
                                                                     "Bartlett Experimental Forest (BART)" = "BART", "Smithsonian Environmental Research Center (SERC)" = "SERC", 
                                                                     "University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", 
                                                                     "The Universtiy of Kansas Field Station (UKFS)" = "UKFS", "Oak Ridge (ORNL)" = "ORNL",
                                                                     "LBJ National Grassland (CLBJ)" = "CLBJ", "Abby Road (ABBY)" = "ABBY", "Toolik Lake (TOOL)" = "TOOL"))
    if (x == "2018" | x == "2019")
      updateCheckboxGroupInput(session, "checkGroup", choices = list ("Harvard Forest (HARV)" = "HARV", "Bartlett Experimental Forest (BART)" = "BART", "Smithsonian Environmental Research Center (SERC)" = "SERC", 
                                                                      "University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", "The Universtiy of Kansas Field Station (UKFS)" = "UKFS", "Oak Ridge (ORNL)" = "ORNL",
                                                                      "LBJ National Grassland (CLBJ)" = "CLBJ", "Abby Road (ABBY)" = "ABBY", "Toolik Lake (TOOL)" = "TOOL",
                                                                      "Caribou Creek (BONA)" = "BONA"))
  })
  
  output$pathPlot <- renderPlot({
    
    # filters data depending on which sites you choose
    temp_select <- temp_data %>%
      filter(year %in% input$select) %>%
      filter(siteID %in% input$checkGroup)
    
    # plot of AGDDs in 2017 for chosen sites as different colors
    ggplot(data=temp_select, aes(x=dayOfYear, y=AGDD, color = siteID)) +
      geom_path() + xlab("Day of Year") + ggtitle("Accumulated Growing Degree Days in Selected Year") +
      scale_colour_manual(values = c("maroon1", "purple", "orange", "navyblue", "cyan",
                                     "brown", "darkcyan", "black", "springgreen1", "red")) + ylab("AGGDs") + ylim(c(0, 6500)) + xlim(c(0,365)) +
       theme_bw() + theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20, hjust = 0.5), 
                          axis.title = element_text(lineheight = .5, size = 15),
                          legend.title = element_text(lineheight = .5, size = 12)) 
  })
  
  
# this second observe and output is for the second tab
  # since some sites have data for years that others don't, depending on what site you choose, 
  # the years with available data are shown for you to choose from
  observe({
    y <- input$select2
    
    if (y == "ORNL" | y == "BART" | y=="HARV" | y=="UNDE")
      updateCheckboxGroupInput(session, "checkGroup2", choices = list("2015"="2015",
                                                                     "2016"="2016", "2017" = "2017", "2018"="2018", "2019"="2019"))
    if (y == "SERC" | y=="UKFS" | y=="ABBY")
      updateCheckboxGroupInput(session, "checkGroup2", choices = list("2016"="2016", "2017" = "2017", "2018"="2018", "2019"="2019"))
    if (y=="CLBJ" | y=="TOOL")
      updateCheckboxGroupInput(session, "checkGroup2", choices = list("2017" = "2017", "2018"="2018", "2019"="2019"))
    if (y=="BONA")
      updateCheckboxGroupInput(session, "checkGroup2", choices = list( "2018"="2018", "2019"="2019"))
  })
  
  
  # the output part for the plot  
  output$pathPlot2 <- renderPlot({
    
    # depending on which site you choose, a dataframe is filtered to only have data from chosen site
    # the data is filtered depending on which years you choose to view
    site_select <- temp_data %>%
      filter(siteID %in% input$select2) %>%
      filter(year %in% input$checkGroup2)
    
    # plots the data with the different years as the different lines
    ggplot(data=site_select, aes(x=dayOfYear, y=AGDD, color = as.factor(year))) +
      geom_path() + xlab("Day of Year") + ggtitle("Accumulated Growing Degree Days Across Years") +
      scale_color_brewer(palette = "Dark2") + ylab("AGGDs")  + xlim(c(0,366))+ theme_bw() +
      theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20, hjust = 0.5), 
            axis.title = element_text(lineheight = .5, size = 15),
            legend.title = element_text(lineheight = .5, size = 12)) + labs(color="Year") 
      
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
