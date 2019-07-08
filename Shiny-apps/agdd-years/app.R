
library(shiny)
library(dplyr)
library(ggplot2)

# reading the dataset included in the agdd-years directory
years_data <- read.csv("years_data.csv", stringsAsFactors = FALSE)

# Define UI 
ui <- fluidPage(

    # Application title
    titlePanel("AGDDs Across Years"),

    # Sidebar 
    # makes you choose a NEON site with a dropdown menu
    # then lets you choose which years to display for the chosen site
    sidebarLayout(
        sidebarPanel(
          # dropdown select menu
            selectInput("select", label = h3("NEON Sites"), choices = list("Harvard Forest (HARV)" = "HARV", "Bartlett Experimental Forest (BART)" = "BART", 
                                        "Smithsonian Environmental Research Center (SERC)" = "SERC", "University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", 
                                        "The Universtiy of Kansas Field Station (UKFS)" = "UKFS", "Oak Ridge (ORNL)" = "ORNL",
                                       "LBJ National Grassland (CLBJ)" = "CLBJ", "Abby Road (ABBY)" = "ABBY", "Toolik Lake (TOOL)" = "TOOL",
                                       "Caribou Creek (BONA)" = "BONA"), selected = "HARV"),
          # checkbox so can pick as many years as you want
            checkboxGroupInput("checkGroup", label = h3("Years"), choices = list("2014" = "2014", "2015"="2015",
                                            "2016"="2016", "2017" = "2017", "2018"="2018", "2019"="2019"), selected = NULL)
            
        ),

        # shows the plot in the main panel
        mainPanel(
           plotOutput("pathPlot")
        )
    )
)


server <- function(input, output, session) {
  
# since some sites have data for years that others don't, depending on what site you choose, 
  # the years with available data are shown for you to choose from
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
  
  
# the output part for the plot  
  output$pathPlot <- renderPlot({
    
  # depending on which site you choose, a dataframe is filtered to only have data from chosen site
    # the data is filtered depending on which years you choose to view
    site_select <- years_data %>%
      filter(siteID %in% input$select) %>%
      filter(year %in% input$checkGroup)
    
    # plots the data with the different years as the different lines
    ggplot(data=site_select, aes(x=dayOfYear, y=AGDD, color = as.factor(year))) +
      geom_path() + xlab("Day of Year") + ggtitle("Accumulated Growing Degree Days Over Years") +
      scale_color_brewer(palette = "Dark2") + ylab("AGGDs")  + xlim(c(0,366))+
      theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20)) + labs(color="Year") +
      theme_bw()
  })
 
  


}

# Run the application 
shinyApp(ui = ui, server = server)
