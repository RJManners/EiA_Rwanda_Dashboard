#enu<-unique(bean_plot$ENID)
# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
  user = c("user1", "user2"),
  password = sapply(c("pass1", "pass2"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)

#read administrative regions
rwa_shp <- rgdal::readOGR(dsn   = "data/data/shp",
                          layer = "rwa_o",
                          stringsAsFactors = FALSE)

rwad_shp <- rgdal::readOGR(dsn   = "data/data/gadm36_RWA_shp",
                           layer = "gadm36_RWA_2",
                           stringsAsFactors = FALSE)

labs <- as.list(rwad_shp$NAME_2)
#basemap for leaflet map
basemap <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lat = -1.88, lng = 29.81, zoom = 8) 

#ggplot theme
them2<-theme(panel.background = element_rect(fill = "white"), # bg of the panel
             plot.background = element_rect(fill = "white", color = NA), # bg of the plot
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             plot.title = element_text(size=12, face="bold",color = "#a9a9a9", hjust = 0.5 ),
             strip.text.x = element_text(size = 15, color = "#a9a9a9", face = "bold"),
             axis.text=element_text(color = "#a9a9a9",size=10),
             axis.text.x = element_text(angle = 60, hjust = 1),
             #axis.text.y = element_blank(),
             #axis.title=element_text(size=16,face="bold"),
             axis.title=element_text(color = "#a9a9a9",size=10),
             legend.title = element_text(color = "#a9a9a9",face="bold", size = 12),
             legend.text = element_text(color = "#a9a9a9", size = 10),                   legend.background = element_rect(fill = "black"),                   panel.border = element_blank(),
             #axis.line.x = element_line(color="black", size = 0.3),
             #scale_x_date(date_breaks = "months" , date_labels = "%b-%y"),
             #axis.line.y = element_line(color="black", size = 0.3))  
             axis.line.x = element_blank(),
             #hovertemplate = paste('%{x}', '<br>lifeExp: %{text:.2s}<br>'),
             axis.line.y = element_blank())




# Function to call in place of dropdownMenu --------------
customSentence <- function(numItems, type) {
  paste("Feedback & suggestions")
  
}



##
dropdownMenuCustom <-     function (..., type = c("messages", "notifications", "tasks"), 
                                    badgeStatus = "primary", icon = NULL, .list = NULL, customSentence = customSentence)
{
  type <- match.arg(type)
  if (!is.null(badgeStatus)) shinydashboard:::validateStatus(badgeStatus)
  items <- c(list(...), .list)
  lapply(items, shinydashboard:::tagAssert, type = "li")
  dropdownClass <- paste0("dropdown ", type, "-menu")
  if (is.null(icon)) {
    icon <- switch(type, messages = shiny::icon("envelope"), 
                   notifications = shiny::icon("warning"), tasks = shiny::icon("tasks"))
  }
  numItems <- length(items)
  if (is.null(badgeStatus)) {
    badge <- NULL
  }
  else {
    badge <- tags$span(class = paste0("label label-", badgeStatus), 
                       numItems)
  }
  tags$li(
    class = dropdownClass, 
    a(
      href = "#", 
      class = "dropdown-toggle", 
      `data-toggle` = "dropdown", 
      icon, 
      badge
    ), 
    tags$ul(
      class = "dropdown-menu", 
      tags$li(
        class = "header", 
        customSentence(numItems, type)
      ), 
      tags$li(
        tags$ul(class = "menu", items)
      )
    )
  )
}




