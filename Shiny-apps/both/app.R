# both
# trying to get phenophases and agdds on same axis before putting it in combined app

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(lubridate)
library(scales)

# load the data, both use the same data
DBL <- read.csv("area.csv", stringsAsFactors = FALSE)
temp_data <- read.csv("years.csv", stringsAsFactors = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
  # Application title
  titlePanel(h2(span("Mapping Heat Accumulation and Phenology Data", style = "color:darkblue"))),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      helpText(h4(strong(span("Select a NEON terrestrial site and choose at least one year. Then select if you want to observe phenology data, AGDDs, or both for the selected site and years.",
                              style = "color:darkblue")))),
      # choose which site you want to observe
      pickerInput("select", label = h3(span("NEON Sites", style = "color:darkblue")), choices = list("Harvard Forest (HARV)" = "HARV", "Bartlett Experimental Forest (BART)" = "BART", 
                                                                                                     "Smithsonian Environmental Research Center (SERC)" = "SERC", "University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", 
                                                                                                     "The Universtiy of Kansas Field Station (UKFS)" = "UKFS", "Oak Ridge (ORNL)" = "ORNL",
                                                                                                     "LBJ National Grassland (CLBJ)" = "CLBJ", "Abby Road (ABBY)" = "ABBY", "Toolik Lake (TOOL)" = "TOOL",
                                                                                                     "Caribou Creek (BONA)" = "BONA"), selected = "HARV"),
      

      
      # checkbox so can pick as many years that are available for that site as you want
      prettyCheckboxGroup("checkGroup", label = h3(span("Years", style = "color:darkblue")), 
                          choices = list("2015"="2015","2016"="2016", "2017" = "2017", "2018"="2018", "2019"="2019"), 
                          selected = NULL, status = "danger", outline = TRUE, inline = TRUE)

      
    ),
    
    
    # Show a plot of the generated distribution
    
    mainPanel(
      plotOutput("dataPlot")
    )
  )
)

# Define server logic required to draw a histogram
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
  
  output$dataPlot <- renderPlot({
    
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
    
    site_select <- temp_data %>%
      filter(siteID %in% input$select) %>%
      filter(year %in% input$checkGroup)
    
    ggplot(data=phenoStat_T, mapping=aes(x = dayOfYear, y = percent, fill = phenophaseName, color = phenophaseName)) +
      geom_density(data=phenoStat_T,alpha=0.3,stat = "identity", position = position_dodge(width = .1)) +
      #geom_density(alpha=0.3,stat = "identity", position = "stack") +  
      theme_bw() + facet_grid(cols = vars(commonName),rows = vars(year), scale = "free_y") +
      #xlab("Day Of Year") + ylab("% of Individuals") + xlim(0,366) +
      #ggtitle("Phenophase Density for Selected Site") +
      #theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20, hjust = 0.5)) +
      #theme(text = element_text(size = 15)) +
      scale_color_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1") +
      theme(legend.position = "bottom") + labs(fill = "Phenophase", color = "Phenophase") +
      geom_path(data=site_select,aes(x=dayOfYear, y=AGDD/240), inherit.aes = FALSE, size = 1.5)+
      scale_y_continuous(sec.axis = sec_axis(~.*240, name = "AGDDs")) +
      labs(x = "Day of Year", y = "Percentage of Individuals in Each Phenophase") 
    
    
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
