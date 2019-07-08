
library(shiny)
library(dplyr)
library(ggplot2)

# reading in the data needed for the app
DBL <- read.csv("DBL.csv", stringsAsFactors = FALSE)

# Define UI 
ui <- fluidPage(

    # Application title
    titlePanel("Phenophases"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
          # drop down menu to pick one year to look at 
            selectInput("select", label = h3("Year"), choices = list("2014"="2014", "2015"="2015", "2016"="2016",
                                 "2017"="2017", "2018"="2018", "2019"="2019"), selected = NULL),
            # drop down to pick one phenophase to observe
            selectInput("select2", label = h3("Phenophase"), choices = list("Breaking Leaf Buds"="Breaking leaf buds",
                            "Increasing Leaf Size"="Increasing leaf size", "Open Flowers"="Open flowers",
                            "Leaves"="Leaves", "Colored Leaves"="Colored leaves", 
                            "Falling Leaves"="Falling leaves"), selected = NULL) ,
            # can choose multiple sites to plot
            checkboxGroupInput("checkGroup", label = h3("NEON Sites"),
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

# Define server logic 
server <- function(input, output, session) {
  
  observe({
    # the year chosen
    x <- input$select
    # the phenophase(s) chosen
    y <- input$select2
    
  # the following code is changing the select options depending on what there is data for
    # some sites don't have data for all the years between 2014 and 2019
    # if a site does have data for a particular year, it doesn't mean they have data for a particular phenophase
    # I changed these options so that you aren't given the option to choose something that won't be plotted
    if (x == "2014"){
      updateCheckboxGroupInput(session, "checkGroup", choices = list("Harvard Forest (HARV)" = "HARV",
                               "Bartlett Experimental Forest (BART)" = "BART","University of Notre Dame Environmental Research Center (UNDE)" = "UNDE"))

      if (y == "Breaking leaf buds" | y == "Open flowers" | y == "Increasing leaf size"){
        updateCheckboxGroupInput(session, "checkGroup", choices = list("Harvard Forest (HARV)" = "HARV",
                      "Bartlett Experimental Forest (BART)" = "BART"))
      }
    }
    
    if (x == "2015"){
      updateCheckboxGroupInput(session, "checkGroup", choices = list("Harvard Forest (HARV)" = "HARV",
                       "Bartlett Experimental Forest (BART)" = "BART","University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", "Oak Ridge (ORNL)" = "ORNL"))

  }
    if (x == "2016"){
      updateCheckboxGroupInput(session, "checkGroup", choices = list("Harvard Forest (HARV)" = "HARV",
                    "Bartlett Experimental Forest (BART)" = "BART","University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", "Oak Ridge (ORNL)" = "ORNL",
                    "Smithsonian Environmental Research Center (SERC)" = "SERC","Abby Road (ABBY)" = "ABBY",
                    "The Universtiy of Kansas Field Station (UKFS)" = "UKFS"))

    }
    if (x == "2017"){
      updateCheckboxGroupInput(session, "checkGroup", choices = list("Harvard Forest (HARV)" = "HARV",
                    "Bartlett Experimental Forest (BART)" = "BART","University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", "Oak Ridge (ORNL)" = "ORNL",
                     "Smithsonian Environmental Research Center (SERC)" = "SERC","Abby Road (ABBY)" = "ABBY",
                    "The Universtiy of Kansas Field Station (UKFS)" = "UKFS","LBJ National Grassland (CLBJ)" = "CLBJ",
                    "Toolik Lake (TOOL)" = "TOOL", "Caribou Creek (BONA)" = "BONA"))

      if (y == "Breaking leaf buds" | y == "Open flowers"){
          updateCheckboxGroupInput(session, "checkGroup", choices = list("Harvard Forest (HARV)" = "HARV",
                   "Bartlett Experimental Forest (BART)" = "BART","University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", "Oak Ridge (ORNL)" = "ORNL",
                   "Smithsonian Environmental Research Center (SERC)" = "SERC","Abby Road (ABBY)" = "ABBY",
                    "The Universtiy of Kansas Field Station (UKFS)" = "UKFS", "LBJ National Grassland (CLBJ)" = "CLBJ"))
      }
      if (y == "Increasing leaf size"){
        updateCheckboxGroupInput(session, "checkGroup", choices = list("Harvard Forest (HARV)" = "HARV",
                 "Bartlett Experimental Forest (BART)" = "BART","University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", "Oak Ridge (ORNL)" = "ORNL",
                  "Smithsonian Environmental Research Center (SERC)" = "SERC","Abby Road (ABBY)" = "ABBY",
                  "The Universtiy of Kansas Field Station (UKFS)" = "UKFS", "LBJ National Grassland (CLBJ)" = "CLBJ",
                 "Caribou Creek (BONA)" = "BONA"))
      }
    }
    if (x == "2018" ){
      updateCheckboxGroupInput(session, "checkGroup", choices = list("Harvard Forest (HARV)" = "HARV",
                  "Bartlett Experimental Forest (BART)" = "BART","University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", "Oak Ridge (ORNL)" = "ORNL",
              "Smithsonian Environmental Research Center (SERC)" = "SERC","Abby Road (ABBY)" = "ABBY",
                 "The Universtiy of Kansas Field Station (UKFS)" = "UKFS","LBJ National Grassland (CLBJ)" = "CLBJ",
                  "Toolik Lake (TOOL)" = "TOOL", "Caribou Creek (BONA)" = "BONA"))

      if (y == "Breaking leaf buds"){
        updateCheckboxGroupInput(session, "checkGroup", choices = list("Harvard Forest (HARV)" = "HARV",
                "Bartlett Experimental Forest (BART)" = "BART","University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", "Oak Ridge (ORNL)" = "ORNL",
               "Smithsonian Environmental Research Center (SERC)" = "SERC","Abby Road (ABBY)" = "ABBY",
                 "The Universtiy of Kansas Field Station (UKFS)" = "UKFS","LBJ National Grassland (CLBJ)" = "CLBJ",
               "Toolik Lake (TOOL)" = "TOOL"))
      }
      if (y == "Increasing leaf size"){
        updateCheckboxGroupInput(session, "checkGroup", choices = list("Harvard Forest (HARV)" = "HARV",
              "Bartlett Experimental Forest (BART)" = "BART","University of Notre Dame Environmental Research Center (UNDE)" = "UNDE",
              "Smithsonian Environmental Research Center (SERC)" = "SERC","Abby Road (ABBY)" = "ABBY",
               "The Universtiy of Kansas Field Station (UKFS)" = "UKFS","LBJ National Grassland (CLBJ)" = "CLBJ",
              "Toolik Lake (TOOL)" = "TOOL", "Caribou Creek (BONA)" = "BONA"))
      }
      if (y == "Open flowers"){
        updateCheckboxGroupInput(session, "checkGroup", choices = list("Harvard Forest (HARV)" = "HARV",
             "Bartlett Experimental Forest (BART)" = "BART","University of Notre Dame Environmental Research Center (UNDE)" = "UNDE",
             "Smithsonian Environmental Research Center (SERC)" = "SERC","Abby Road (ABBY)" = "ABBY",
            "The Universtiy of Kansas Field Station (UKFS)" = "UKFS","LBJ National Grassland (CLBJ)" = "CLBJ",
             "Toolik Lake (TOOL)" = "TOOL"))
      }
    }
    if (x == "2019"){
      updateCheckboxGroupInput(session, "checkGroup", choices = list("Harvard Forest (HARV)" = "HARV",
                 "Bartlett Experimental Forest (BART)" = "BART","University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", "Oak Ridge (ORNL)" = "ORNL",
                  "Smithsonian Environmental Research Center (SERC)" = "SERC","Abby Road (ABBY)" = "ABBY",
                 "The Universtiy of Kansas Field Station (UKFS)" = "UKFS","LBJ National Grassland (CLBJ)" = "CLBJ",
               "Toolik Lake (TOOL)" = "TOOL", "Caribou Creek (BONA)" = "BONA"))

      if (y == "Breaking leaf buds"){
        updateCheckboxGroupInput(session, "checkGroup", choices = list("Harvard Forest (HARV)" = "HARV",
               "University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", "Oak Ridge (ORNL)" = "ORNL",
                "Smithsonian Environmental Research Center (SERC)" = "SERC","Abby Road (ABBY)" = "ABBY",
               "The Universtiy of Kansas Field Station (UKFS)" = "UKFS", "Caribou Creek (BONA)" = "BONA"))
      }
      if (y == "Increasing leaf size" | y == "Leaves"){
        updateCheckboxGroupInput(session, "checkGroup", choices = list("Harvard Forest (HARV)" = "HARV",
             "Oak Ridge (ORNL)" = "ORNL", "Smithsonian Environmental Research Center (SERC)" = "SERC",
             "Abby Road (ABBY)" = "ABBY", "The Universtiy of Kansas Field Station (UKFS)" = "UKFS"))
      }
      if (y == "Open flowers"){
        updateCheckboxGroupInput(session, "checkGroup", choices = list("Harvard Forest (HARV)" = "HARV",
               "Bartlett Experimental Forest (BART)" = "BART","University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", "Oak Ridge (ORNL)" = "ORNL",
                "Smithsonian Environmental Research Center (SERC)" = "SERC","Abby Road (ABBY)" = "ABBY",
            "The Universtiy of Kansas Field Station (UKFS)" = "UKFS"))
      }
      if (y == "Colored leaves" | y == "Falling leaves")
        updateCheckboxGroupInput(session, "checkGroup", choices = character(0) )
    }
  })

  # code for the plot
    output$phenoPlot <- renderPlot({
      
      # filtering the data depending on what was chosen
      phenoSelect <- DBL %>%
        filter(year %in% input$select) %>%
        filter(phenophaseName %in% input$select2) %>%
        filter(siteID %in% input$checkGroup)
      
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
        xlab("Day Of Year") + ylab("% of Individuals") + xlim(0,366) + ylim(0,100) +
        theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20)) +
        theme(text = element_text(size = 18)) +
        scale_color_brewer(palette = "Paired")
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
