# comp-one

library(shiny)

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Comparing One Phenophase and AGGDs"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            selectInput("select", label = h3(span("NEON Sites", style = "color:darkblue")),
                    choices = list("Harvard Forest (HARV)" = "HARV", "Bartlett Experimental Forest (BART)" = "BART",
                    "Smithsonian Environmental Research Center (SERC)" = "SERC", "University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", 
                    "The Universtiy of Kansas Field Station (UKFS)" = "UKFS", "Oak Ridge (ORNL)" = "ORNL",
                    "LBJ National Grassland (CLBJ)" = "CLBJ", "Abby Road (ABBY)" = "ABBY", "Toolik Lake (TOOL)" = "TOOL",
                    "Caribou Creek (BONA)" = "BONA")),
            selectInput("select2", label = h3(span("Years", style = "color:darkblue")), 
                        choices = list("2015"="2015", "2016"="2016", "2017"="2017", "2018"="2018", "2019"="2019")),
            selectInput("select3", label = h3(span("Phenophase", style = "color:darkblue")),
                      choices = list("Breaking Leaf Buds"="Breaking leaf buds","Increasing Leaf Size"="Increasing leaf size", 
                                     "Open Flowers"="Open flowers","Leaves"="Leaves", "Colored Leaves"="Colored leaves", 
                                       "Falling Leaves"="Falling leaves"))
        ),

        # Show a plot 
        mainPanel(
           plotOutput("compPlot")
        )
    )
)

# Define server logic required
server <- function(input, output, session) {

    output$compPlot <- renderPlot({
      
      observe({
        x <- input$select
        y <- input$select2
        
        if (x == "ORNL"){
          updateSelectInput(session, "select2",  choices = list("2015"="2015","2016"="2016", "2017" = "2017", "2018"="2018", "2019"="2019"))
          if (y == "2018")
            updateSelectInput(session, "select3", choices = list("Breaking Leaf Buds"="Breaking leaf buds","Leaves"="Leaves", "Colored Leaves"="Colored leaves", 
                                                                 "Falling Leaves"="Falling leaves"))
       }
        if (x == "BART"){
          updateSelectInput(session, "select2",  choices = list("2015"="2015","2016"="2016", "2017" = "2017", "2018"="2018", "2019"="2019"))
          if (y == "2019")
            updateSelectInput(session, "select3", choices = list( "Open Flowers"="Open flowers","Colored Leaves"="Colored leaves", 
                                                                 "Falling Leaves"="Falling leaves"))
       }
        if ( x=="HARV"){
          updateSelectInput(session, "select2",  choices = list("2015"="2015","2016"="2016", "2017" = "2017", "2018"="2018", "2019"="2019"))
       }
        if (x=="UNDE"){
          updateSelectInput(session, "select2",  choices = list("2015"="2015","2016"="2016", "2017" = "2017", "2018"="2018", "2019"="2019"))
          if (y == "2019")
            updateSelectInput(session, "select3", choices = list("Breaking Leaf Buds"="Breaking leaf buds", "Open Flowers"="Open flowers",
                                                                 "Colored Leaves"="Colored leaves", "Falling Leaves"="Falling leaves"))
       }
        if (x == "SERC" | x=="UKFS" | x=="ABBY"){
          updateSelectInput(session, "select2", choices = list("2016"="2016", "2017" = "2017", "2018"="2018", "2019"="2019"))
       }
        if (x=="CLBJ"){
          updateSelectInput(session, "select2", choices = list("2017" = "2017", "2018"="2018"))
        }
        if (x=="TOOL"){
          updateSelectInput(session, "select2", choices = list("2017" = "2017", "2018"="2018"))
          if (y == "2017")
            updateSelectInput(session, "select3", choices = list("Leaves"="Leaves", "Colored Leaves"="Colored leaves", "Falling Leaves"="Falling leaves"))
        }
        if (x=="BONA"){
          updateSelectInput(session, "select2", choices = list( "2018"="2018", "2019"="2019"))
          if (y == "2017" | y == "2018")
            updateSelectInput(session, "select3",choices = list("Increasing Leaf Size"="Increasing leaf size", 
                                                                "Leaves"="Leaves", "Colored Leaves"="Colored leaves", 
                                                                "Falling Leaves"="Falling leaves"))
          if (y == "2019")
            updateSelectInput(session, "select3", choices = list("Breaking Leaf Buds"="Breaking leaf buds", "Colored Leaves"="Colored leaves", 
                                                                 "Falling Leaves"="Falling leaves"))
       } 
      })

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
