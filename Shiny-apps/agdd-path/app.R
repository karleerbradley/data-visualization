
library(shiny)
library(dplyr)
library(ggplot2)


# reading the data into app
temp_data <- read.csv("temp_data.csv", stringsAsFactors = FALSE)

# Define UI 
ui <- fluidPage(

    # Application title
    titlePanel("2017 AGDD Paths"),

    # Sidebar
    # has a checkbox menu for you to choose whichever NEON sites to display data for
    sidebarLayout(
        sidebarPanel(
          # HARV is chosen as a default
            checkboxGroupInput("checkGroup",
                        label = h3("NEON Sites"), 
                        choices = list("Harvard Forest (HARV)" = "HARV", "Bartlett Experimental Forest (BART)" = "BART", "Smithsonian Environmental Research Center (SERC)" = "SERC", 
                                       "University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", "The Universtiy of Kansas Field Station (UKFS)" = "UKFS", "Oak Ridge (ORNL)" = "ORNL",
                                       "LBJ National Grassland (CLBJ)" = "CLBJ", "Abby Road (ABBY)" = "ABBY", "Toolik Lake (TOOL)" = "TOOL",
                                       "Caribou Creek (BONA)" = "BONA"), selected = "HARV")
        ),

        # Show a plot 
        mainPanel(
           plotOutput("pathPlot")
        )
    )
)

# Define server 
server <- function(input, output) {
    
    output$pathPlot <- renderPlot({
      
        # filters data depending on which sites you choose
        temp_select <- temp_data %>%
            filter(siteID %in% input$checkGroup)
        
        # plot of AGDDs in 2017 for chosen sites as different colors
        ggplot(data=temp_select, aes(x=dayOfYear, y=AGDD, color = siteID)) +
            geom_path() + xlab("Day of Year") + ggtitle("Accumulated Growing Degree Days in 2017") +
            scale_color_brewer(palette = "Paired") + ylab("AGGDs") + ylim(c(0, 6500)) + xlim(c(0,365))+
            theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20)) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
