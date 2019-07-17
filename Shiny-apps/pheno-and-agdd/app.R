# pheno-and-agdd
# meadowlark-style with agdd on top
# tried to make buttons that would plot only phenophases, only agdds, but not working with if statements
# how to make the top panel shorter so that the graph can take up most of the room??

library(shiny)
library(shinyWidgets)
library(htmltools)
library(ggplot2)
library(dplyr)
library(lubridate)

# load the data, both use the same data
DBL <- read.csv("area.csv", stringsAsFactors = FALSE)
temp_data <- read.csv("years.csv", stringsAsFactors = FALSE)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(h3(span("Mapping Heat Accumulation and Phenology Data", style = "color:darkblue")), div(style="padding: 0px 0px; margin:0%")),hr(),

    # Sidebar 
    fluidRow(div(style="height:.2px "),
        column(12,div(style = "padding: 0px 0px; margin-top:-1em"),
          helpText(h6(span("Select a NEON terrestrial site and choose at least one year. Then select if you want to observe phenology data, AGDDs, or both for the selected site and years.",
                                  style = "color:darkblue;"))))
    ),
    fluidRow(
         column(4, style="height:75px",
          # choose which site you want to observe
          pickerInput("select", label = h5(span("NEON Sites", style = "color:darkblue")), choices = list("Harvard Forest (HARV)" = "HARV", "Bartlett Experimental Forest (BART)" = "BART", 
                                                                                                         "Smithsonian Environmental Research Center (SERC)" = "SERC", "University of Notre Dame Environmental Research Center (UNDE)" = "UNDE", 
                                                                                                         "The Universtiy of Kansas Field Station (UKFS)" = "UKFS", "Oak Ridge (ORNL)" = "ORNL",
                                                                                                         "LBJ National Grassland (CLBJ)" = "CLBJ", "Abby Road (ABBY)" = "ABBY", "Toolik Lake (TOOL)" = "TOOL",
                                                                                                         "Caribou Creek (BONA)" = "BONA"), selected = "HARV")
          ),

        column(4,style="height:75px",
         # choose if you want to look at just phenophases, just AGDDs, or both using radio buttons
         # radioGroupButtons("option", label = h5(span("Choose what to observe", style = "color:darkblue")), 
         #                   choices = c("Phenophases"="phenos", "AGDDS"="agdd", "Phenophases & AGDDS"="both"), individual = TRUE, 
         #                   checkIcon = list(
         #                     yes = tags$i(icon("check"), 
         #                                  style = "color:firebrick")
         #                     ))
         # can choose if you want to look at phenophases, AGDDs, or both using checkbox buttons
         prettyRadioButtons("option", label = h5(span("Choose what to observe", style = "color:darkblue")), choices = c("Phenophases" ="phenos","AGDDs"="agdd", "Phenophases & AGDDs"="both"),
                             selected = "phenos", status = "danger", outline = TRUE, inline = TRUE)
        ),
          column(4, style="height:75px",
          # checkbox so can pick as many years that are available for that site as you want
          prettyCheckboxGroup("checkGroup", label = h5(span("Years", style = "color:darkblue")), 
                             choices = list("2015"="2015","2016"="2016", "2017" = "2017", "2018"="2018", "2019"="2019"), 
                             selected = NULL, status = "danger", outline = TRUE, inline = TRUE))
          
          #,br(),
         # button to plot that you have to click again to refresh the graph
          #actionButton("goplot", label = "PLOT")
          
        ), hr(),
        

  # depending on what the user chooses to observe, the following plots are made
        mainPanel(
           #plotOutput("dataPlot")
          conditionalPanel(condition = "input.option == 'agdd'", plotOutput("yearsPlot", width = "150%", height = "500px")),
          conditionalPanel(condition = "input.option == 'phenos'", plotOutput("phenoPlot", width = "150%", height = "500px")),
          conditionalPanel(condition = "input.option == 'both'", plotOutput("bothPlot", width = "150%", height = "500px"))
          # maybe this is how you plot if wanted to use checkbox buttons instead of radio buttons for option
          # conditionalPanel(condition = "input.option == 'agdd' & input.option=='phenos'", plotOutput("bothPlot"))
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

  
  
 # if the user chooses to observe agdds, then this plot is generated
 output$yearsPlot <- renderPlot({

      # depending on which site you choose, a dataframe is filtered to only have data from chosen site
      # the data is filtered depending on which years you choose to view
      site_select <- temp_data %>%
        filter(siteID %in% input$select) %>%
        filter(year %in% input$checkGroup)

      # plots the data with the different years as the different lines
      ggplot(data=site_select, aes(x=dayOfYear, y=AGDD, color = as.factor(year))) +
        geom_path() + xlab("Day of Year") + ggtitle("Accumulated Growing Degree Days Across Years") +
        scale_color_brewer(palette = "Dark2") + ylab("AGGDs")  + xlim(c(0,366))+ theme_bw() +
        theme(plot.title = element_text(lineheight = .8, face = "bold", size = 30, hjust = 0.5),
              axis.title = element_text(lineheight = .8, size = 20),
              legend.title = element_text(lineheight = .8, size = 20),
              legend.text = element_text(size = 15), 
              axis.text = element_text(size = 15)) + labs(color="Year")
 })
   
    
# if the user chooses to observe phenophases, then this plot is generated
 output$phenoPlot <- renderPlot({

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
      theme(plot.title = element_text(lineheight = .8, face = "bold", size = 30, hjust = 0.5),
            axis.title = element_text(lineheight = .8, size = 20),
            legend.title = element_text(lineheight = .8, size = 20),
            legend.text = element_text(size = 15), 
            axis.text = element_text(size = 15), strip.text.x = element_text(size = 15), strip.text.y = element_text(size = 15)) +
      scale_color_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1") +
      theme(legend.position = "bottom") + labs(fill = "Phenophase", color = "Phenophase")

 })
    
 
 
# if the user chooses to observe both phenophases and agdds, then this plot is generated 
    output$bothPlot <- renderPlot({
      
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
        scale_color_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1") +
        theme(legend.position = "bottom") + labs(fill = "Phenophase", color = "Phenophase") +
        geom_path(data=site_select,aes(x=dayOfYear, y=AGDD/240), inherit.aes = FALSE, size = 1.5)+
        scale_y_continuous(sec.axis = sec_axis(~.*240, name = "AGDDs")) +
        labs(x = "Day of Year", y = "Percentage of Individuals\nin Each Phenophase") + ggtitle("Phenophases and AGDDs") +
        theme(plot.title = element_text(lineheight = .8, face = "bold", size = 30, hjust = 0.5),
              axis.title = element_text(lineheight = .8, size = 20),
              legend.title = element_text(lineheight = .8, size = 20),
              legend.text = element_text(size = 15), 
              axis.text = element_text(size = 15),
              strip.text.x = element_text(size = 15), strip.text.y = element_text(size = 15))

      
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
