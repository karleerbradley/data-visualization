# pheno-and-agdd
# meadowlark-style with agdd on top
# tried to make buttons that would plot only phenophases, only agdds, but not working with if statements

library(shiny)
library(shinyWidgets)
library(htmltools)
library(ggplot2)

# load the data, both use the same data
DBL <- read.csv("area.csv", stringsAsFactors = FALSE)
temp_data <- read.csv("years.csv", stringsAsFactors = FALSE)


# Define UI for application that draws a histogram
ui <- fluidPage(

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
          
          # can choose if you want to look at phenophases, AGDDs, or both using checkbox buttons
         # prettyCheckboxGroup("option", label = h3(span("Choose what to observe", style = "color:darkblue")), choices = c("Phenophases" ="phenos","AGDDs"="agdd"),
         #                     selected = "phenos", status = "danger", outline = TRUE, inline = TRUE),
        
         # choose if you want to look at just phenophases, just AGDDs, or both using radio buttons
         radioGroupButtons("option", label = h3(span("Choose what to observe", style = "color:darkblue")), 
                           choices = c("Phenophases"="phenos", "AGDDS"="agdd", "Phenophases & AGDDS"="both"), individual = TRUE, 
                           checkIcon = list(
                             yes = tags$i(icon("check"), 
                                          style = "color:firebrick")
                             )),
          
          # checkbox so can pick as many years that are available for that site as you want
          prettyCheckboxGroup("checkGroup", label = h3(span("Years", style = "color:darkblue")), 
                             choices = list("2015"="2015","2016"="2016", "2017" = "2017", "2018"="2018", "2019"="2019"), 
                             selected = NULL, status = "danger", outline = TRUE, inline = TRUE)
          
          #,br(),
         # button to plot that you have to click again to refresh the graph
          #actionButton("goplot", label = "PLOT")
          
        ),
        

  # depending on what the user chooses to observe, the following plots are made
        mainPanel(
           #plotOutput("dataPlot")
          conditionalPanel(condition = "input.option == 'agdd'", plotOutput("yearsPlot")),
          conditionalPanel(condition = "input.option == 'phenos'", plotOutput("phenoPlot")),
          conditionalPanel(condition = "input.option == 'both'", plotOutput("bothPlot"))
          # maybe this is how you plot if wanted to use checkbox buttons instead of radio buttons for option
          # conditionalPanel(condition = "input.option == 'agdd'" & "input.option == 'phenos'", plotOutput("bothPlot"))
        )
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



  #output$dataPlot <- renderPlot({
  
  
 # if the user chooses to observe agdds, then this plot is generated
 output$yearsPlot <- renderPlot({
    #if (input$option == "agdd"){
 # if ("agdd" %in% input$option){
      # depending on which site you choose, a dataframe is filtered to only have data from chosen site
      # the data is filtered depending on which years you choose to view
      site_select <- temp_data %>%
        filter(siteID %in% input$select) %>%
        filter(year %in% input$checkGroup)

      # plots the data with the different years as the different lines
      ggplot(data=site_select, aes(x=dayOfYear, y=AGDD, color = as.factor(year))) +
        geom_path() + xlab("Day of Year") + ggtitle("Accumulated Growing Degree Days Across Years") +
        scale_color_brewer(palette = "Dark2") + ylab("AGGDs")  + xlim(c(0,366))+ theme_bw() +
        theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20, hjust = 0.5),
              axis.title = element_text(lineheight = .5, size = 15),
              legend.title = element_text(lineheight = .5, size = 12)) + labs(color="Year")

  #  }
    
 })
   
    
# if the user chooses to observe phenophases, then this plot is generated
 output$phenoPlot <- renderPlot({
 
  #if (input$option == "phenos"){
    #if ("phenos" %in% input$option){
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
      geom_density(alpha=0.3,stat = "identity", position = position_dodge(width = .1)) +
      #geom_density(alpha=0.3,stat = "identity", position = "stack") +
      theme_bw() + facet_grid(cols = vars(commonName),rows = vars(year), scale = "free_y") +
      xlab("Day Of Year") + ylab("% of Individuals") + xlim(0,366) +
      ggtitle("Phenophase Density for Selected Site") +
      theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20, hjust = 0.5)) +
      theme(text = element_text(size = 15)) +
      scale_color_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1") +
      theme(legend.position = "bottom") + labs(fill = "Phenophase", color = "Phenophase")
  #}
    
 })
    
 
 
# if the user chooses to observe both phenophases and agdds, then this plot is generated 
    output$bothPlot <- renderPlot({
      
      
      
      
    })

    
    



}

# Run the application 
shinyApp(ui = ui, server = server)
