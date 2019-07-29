library(plotly)
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1Ijoia2JyYWRsZTEiLCJhIjoiY2p5b3JrcG1yMDFvazNibzJ0bHMzcGdiayJ9.y4HFULDWOBCcT9hA6Pr9hg')

field_sites <- read.csv(file = "field-sites.csv", stringsAsFactors = FALSE)

sites <- c("HARV", "SERC", "UNDE", "BONA", "BART", "UKFS", "ORNL", "CLBJ","ABBY", "TOOL")

field_sites <- field_sites %>%
  group_by(Site.ID) %>%
  filter(Site.ID %in% sites)

map <- field_sites %>%
  plot_mapbox(lat = ~Latitude, lon = ~Longitude, split = ~Site.ID, text = ~Site.Name,  size = 3, color = I("magenta"),
                   mode = "scattermapbox", hoverinfo = "text") %>%
  config(displayModeBar = FALSE) %>%
  layout(title = 'NEON Field Sites with Deciduous Broadleaf Trees',
         font = list(color='black'),
         plot_bgcolor = 'white', paper_bgcolor = 'white',
         mapbox = list(style = 'satellite-streets', center = list(lat = 50,
                                                      lon =-110), 
                       zoom = 1.7),
         legend = list(orientation = 'h',
                       font = list(size = 8)),
         margin = list(l = 0, r = 0,
                       b = 0, t = 25,
                       pad = 0), showlegend = FALSE, hoverlabel = list(bgcolor = "white", font = list(color = "black"), bordercolor = "magenta"))
map
