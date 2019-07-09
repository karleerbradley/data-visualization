
library(shiny)
library(dplyr)
library(ggplot2)


# reading the data into app
temp_data <- read.csv("years_dataPath.csv", stringsAsFactors = FALSE)

# Define UI 
ui <- fluidPage(

    # Application title
    titlePanel("Yearly AGDD Paths"),

    # Sidebar
    # has a checkbox menu for you to choose whichever NEON sites to display data for
    sidebarLayout(
        sidebarPanel(
          selectInput("select", label = h3("Years"), choices = list("2015"="2015", "2016"="2016", 
                                      "2017"="2017","2018"="2018", "2019"="2019")),
          # HARV is chosen as a default
            checkboxGroupInput("checkGroup",
                        label = h3("NEON Sites"), 
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
)

# Define server 
server <- function(input, output, session) {
  
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
          scale_colour_manual(values = c("maroon1", "purple", "orange", "navyblue", "darkcyan",
                                         "brown", "cyan", "springgreen1", "black", "red")) + ylab("AGGDs") + ylim(c(0, 6500)) + xlim(c(0,365))+
            theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20)) + theme_bw()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
