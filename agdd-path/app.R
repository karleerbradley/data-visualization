
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("2017 AGDD Paths"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("checkGroup",
                        label = h3("NEON Sites"), 
                        choices = list("Harvard Forest" = "HARV", "Bartlett Experimental Forest" = "BART", "Smithsonian Environmental Research Center" = "SERC", 
                                       "UNDERC" = "UNDE", "The Universtiy of Kansas Field Station" = "UKFS", "Oak Ridge" = "ORNL",
                                       "LBJ National Grassland" = "CLBJ", "Abby Road" = "ABBY", "Toolik Lake" = "TOOL",
                                       "Caribou Creek" = "BONA"), selected = "HARV")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("pathPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$pathPlot <- renderPlot({
        
        temp_select <- day_temp_2 %>%
            filter(siteID %in% input$checkGroup)
        
        ggplot(data=temp_select, aes(x=dayOfYear, y=AGDD, color = siteID)) +
            geom_path() + xlab("Day of Year") + ggtitle("Accumulated Growing Degree Days in 2017") +
            scale_color_brewer(palette = "Paired") + ylab("AGGDs") + ylim(c(0, 6500)) + xlim(c(0,365))+
            theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20)) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
