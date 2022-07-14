
library(shiny)
library(shinyauthr)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(lubridate)
library(stringr)
library(plotly)
library(shinyBS)
library(shinyjs)
library(leaflet)
library(shinyalert)
library(magrittr)
library(shinycssloaders)
library(ona)
library(magrittr)
library(reactable)

#source('ona.R')

#source('Sandbox.R')

## load functions
source('support_fun.R')


#enu<-unique(bean_plot$ENID)
# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
  user = c("user1", "user2"),
  password = sapply(c("pass1", "pass2"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)

#source script with data import and dataprep 
#source('Sandbox.R')

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
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView(lat = -1.88, lng = 29.81, zoom = 8) 

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


# Define UI for application that draws a histogram
ui <- fluidPage(
  
    #shinyauthr::logoutUI(id = "logout"),
  
  # Sidebar to show user info after login
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  
  # login section
  shinyauthr::loginUI(id = "login"),
  
  uiOutput("sidebarpanel")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Logout to hide
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  output$sidebarpanel <- renderUI({
    # Show only when authenticated
    req(credentials()$user_auth)
    
    
    header<-
      
      
      dashboardHeader( 
        
        title =  list(tags$img(src='logo/logo-3.png', align='left'), tags$h3("Data Monitoring Dashboard")), 
                       disable = FALSE, 
                       titleWidth  = 550,
                       dropdownMenuCustom( type = 'message',
                                           customSentence = customSentence,
                                           messageItem(
                                             from = "",#'Feedback and suggestions',
                                             message =  "",
                                             icon = icon("envelope"),
                                             href = "mailto:R.Manners@cgiar.org"
                                           ),
                                           icon = icon('comment')
                       )
                       
                       
                       
      )
    
    
    
    sidebar<-
      
      
      ## 2. siderbar ------------------------------
    
    dashboardSidebar( 
      width = 200,
      sidebarMenu(
        
        id = 'sidebar',
        style = "position: relative; overflow: visible;",
        
        
        menuItem( "Data Val", tabName = 'dashboard', icon = icon('dashboard'),startExpanded = T
                  
        ),
        #filter options
        selectInput(
          "cropfinder",
          label = "Crop",
          multiple=FALSE,
          choices = list("Beans"="beans","Cassava"="cassava","Potato"="potato", "Maize"="maize", "Rice"="rice","Wheat"="wheat"),
          selected= "Beans"),
        
        selectInput(
          "seasonfinder",
          label = "Season",
          multiple=FALSE,
          choices = list("A"="A","B"="B"),
          selected= "B"),
        
        # selectInput(
        #   "enumeratorfinder",
        #   label = "Enumerator",
        #   multiple=FALSE,
        #   choices = list("All"="all", "EA"="A","EB"="B"),
        #   selected= "EB"),
        
        
        uiOutput('enumeratorfinderr'),
        
        ## Show panel only when ci sidebar is selected
        useShinyjs(),
        
        
        ## 'Glossary' tab ----------------------
        menuItem( "Glossary", tabName = 'glossary', icon = icon('bell') 
                  
        ),
        
        ## 'About' tab ----------------------
        menuItem( "About", tabName = 'about', icon = icon('question-circle')
                  #badgeLabel = "", badgeColor = "green" 
        )
      )
    )
    
    body<-
      ## 3. body --------------------------------
    dashboardBody( 
      
      ## 3.0. CSS styles in header ----------------------------
      list(
                #tags$script(src = "world.js" ),
        tags$script("document.title = 'Data Monitoring Dashboard'"),
        
        ### Styles 
        tags$style(HTML(".small-box {height: 65px}")),
        tags$style(HTML(".fa { font-size: 35px; }")),
        tags$style(HTML(".glyphicon { font-size: 33px; }")),  ## use glyphicon package
        tags$style(HTML(".fa-dashboard { font-size: 20px; }")),
        # tags$style(HTML(".fa-globe { font-size: 20px; }")),
        # tags$style(HTML(".fa-barcode { font-size: 20px; }")),
        # tags$style(HTML(".tab-content { padding-left: 20px; padding-right: 30px; }")) ,
        # tags$style(HTML(".fa-wrench { font-size: 15px; }")),
        # tags$style(HTML(".fa-refresh { font-size: 15px; }")),
        # tags$style(HTML(".fa-search { font-size: 15px; }")),
        tags$style(HTML(".fa-comment { font-size: 20px; }")),
        # tags$style(HTML(".fa-share-alt { font-size: 20px; }")),
        tags$style(HTML(".fa-envelope { font-size: 20px; }")),
        tags$style(HTML(".fa-question-circle { font-size: 20px; }")),
        tags$style(HTML(".fa-chevron-circle-down { font-size: 15px; }")),
        tags$style(HTML(".fa-bell { font-size: 17px; }")),
        # tags$style(HTML(".fa-check { font-size: 14px; }")),
        # tags$style(HTML(".fa-times { font-size: 14px; }")),
        
        
        ## modify the dashboard's skin color  #006272 #494a4a
        ## modify the dashboard's skin color
        tags$style(HTML("
        
                       /* logo */
                       .logo {
                       background-color: #006272;
                       }
                       .main-header {
                       background-color: #006272;
                       }

                       /* logo when hovered */
                       .logo:hover {
                       background-color: #006272;
                       }

                       /* navbar (rest of the header) */
                       .navbar {
                       background-color: #006272;
                       }
                       /* sidebar */
                       .sidebar {
                       background-color: #222d32;
                       }
                       /* sidebar */
                       .main-sidebar {
                       background-color: #222d32;
                       }

                       /* active selected tab in the sidebarmenu */
                       .main-sidebar .sidebar .sidebar-menu .active a{
                       background-color: #006272;
                                 }
                       ")
        ),
        
        ## modify icon size in the sub side bar menu
        tags$style(HTML('
                       /* change size of icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview-menu>li>a>.fa {
                      font-size: 15px;
                      }

                      .sidebar .sidebar-menu .treeview-menu>li>a>.glyphicon {
                      font-size: 13px;
                      }

                      /* Hide icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview>a>.fa-angle-left {
                      display: none;
                      } '
                        
        )) ,
        
        tags$style( HTML("hr {border-top: 1px solid #000000;}") ),
        
        ## to not show error message in shiny
        tags$style( HTML(".shiny-output-error { visibility: hidden; }") ),
        tags$style( HTML(".shiny-output-error:before { visibility: hidden; }") ),
        
        ## heand dropdown menu size
        #tags$style(HTML('.navbar-custom-menu>.navbar-nav>li>.dropdown-menu { width:100px;}'))
        tags$style(HTML('.navbar-custom-menu>.navbar-nav>li:last-child>.dropdown-menu { width:10px; font-size:10px; padding:1px; margin:1px;}')),
        tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > h4 {width:0px; font-size:0px; padding:0px; margin:0px;}')),
        tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > p {width:0px; font-size:0px; padding:0px; margin:0px;}'))
      ),
      
      
      ## 3.1 Dashboard body --------------
      tabItems(
        ## 3.1 Main dashboard ----------------------------------------------------------
        
        
        
        tabItem( tabName = 'dashboard',
                 
                 
                 
                 tabsetPanel(
                   id = "tabs", type = "tabs",
                   
                   tabPanel("Summary",
                            fluidRow( column( width = 6,h4("Trials by Location", align = 'center'), leafletOutput('trials_map') ),
                                      column( width = 6,h4("Trend of Submissions", align = 'center'), plotlyOutput('submission_trend') )
                            ),
                            
                   ),
                   
                   tabPanel("Enumerators", 
                            
                            reactableOutput("tableR")
                            
                            
                   ),
                   tabPanel("Households",
                            reactableOutput("tableH")
                            
                   ),
                   
                   tabPanel("Plots"
                            
                   ),
                   tabPanel("Data Preview",
                            
                            column(12,
                                   dataTableOutput('table1'),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                            ),
                            downloadButton("download", "Download csv")
                   ),
                 )
                 
                 
                 
                 
        ),
        
        
        
        ## 3.5 Help and info -------------------------------
        tabItem( tabName = 'glossary',
                 ## 3.5.1 Data desc/sources ---------------

        ),

        ## 3.6 Monthly update from Stats NZ --------------
        tabItem( tabName = 'about',
                 
                 )
      )
    )
    
    
    fluidPage(
      
      dashboardPage(header, sidebar, body)
    )
    
    # 
    
    
    
  })
  
  #shinyalert("Welcome", "Welcome to the RAB Data Validation Dashboard! Log in below", type = "")
  
  
  
  # output$enumeratorfinderr <- renderUI({
  #   
  #   selectInput(
  #     "enumeratorfinder",
  #     #label =  HTML('<p style="font-weight: bold;  color:white">Sector</p>'),
  #     multiple=FALSE,
  #     choices = list("All",as.character(bean_plot[which(bean_plot$ENID == input$enumeratorfinder),"ENID" ])),
  #     selected= "All")
  #   
  #   
  # })
  
  #Update enumerator selectable list by crop
  observeEvent(input$cropfinder,{
    if (input$cropfinder=="beans"){
      datacrop<-bean_plot
    }else if (input$cropfinder=="cassava"){
      datacrop<-cassava_plot
    }else if (input$cropfinder=="maize"){
      datacrop<-maize_plot
    }else if (input$cropfinder=="rice"){
      datacrop<-rice_plot
    }else if (input$cropfinder=="wheat"){
      datacrop<-wheat_plot
    }else if (input$cropfinder=="potato"){
      datacrop<-potato_plot
    }else {
      datacrop<-bean_plot
    }
    
    output$enumeratorfinderr <- renderUI({
      selectInput(
        "enumeratorfinder",
        label = "Enumerator",
        #label =  HTML('<p style="font-weight: bold;  color:white">Sector</p>'),
        multiple=FALSE,
        choices = c("All",unique(datacrop$ENID)),
        selected= "All")
    })
  })
  
  
  
  #server
  
  output$trials_map <- renderLeaflet({
    basemap
  })
  
  toListen <- reactive({
    list(input$cropfinder,input$seasonfinder,input$enumeratorfinder)
  })
  
  observeEvent(toListen(),{
    
    if ( "beans" %in% input$cropfinder && "B" %in% input$seasonfinder){
      
        if ("All" %in% input$enumeratorfinder){
          datamap<-bean_plot
          output$table1 <- renderDataTable(bean_plot)
        }else{
          datamap<-bean_plot[which(bean_plot$ENID==input$enumeratorfinder), ]
          output$table1 <- renderDataTable(bean_plot)
        }
      
    }else if ("cassava" %in% input$cropfinder && "B" %in% input$seasonfinder){
      
      if ("All" %in% input$enumeratorfinder){
        datamap<-cassava_plot
        output$table1 <- renderDataTable(cassava_plot)
      }else{
        datamap<-cassava_plot[which(cassava_plot$ENID==input$enumeratorfinder), ]
        output$table1 <- renderDataTable(cassava_plot)
      }
      
      
    }else if ("wheat" %in% input$cropfinder && "B" %in% input$seasonfinder){
      
      if ("All" %in% input$enumeratorfinder){
        datamap<-wheat_plot
        output$table1 <- renderDataTable(wheat_plot)
      }else{
        datamap<-wheat_plot[which(wheat_plot$ENID==input$enumeratorfinder), ]
        output$table1 <- renderDataTable(wheat_plot)
      }
      
    }else if ("maize" %in% input$cropfinder && "B" %in% input$seasonfinder){
      
      if ("All" %in% input$enumeratorfinder){
        datamap<-maize_plot
        output$table1 <- renderDataTable(maize_plot)
      }else{
        datamap<-maize_plot[which(maize_plot$ENID==input$enumeratorfinder), ]
        output$table1 <- renderDataTable(maize_plot)
      }
      
    }else if ("potato" %in% input$cropfinder && "B" %in% input$seasonfinder){
      
      if ("All" %in% input$enumeratorfinder){
        datamap<-potato_plot
        output$table1 <- renderDataTable(potato_plot)
      }else{
        datamap<-potato_plot[which(potato_plot$ENID==input$enumeratorfinder), ]
        output$table1 <- renderDataTable(potato_plot)
      }
      
    }else if ("rice" %in% input$cropfinder && "B" %in% input$seasonfinder){
      
      if ("All" %in% input$enumeratorfinder){
        datamap<-rice_plot
        output$table1 <- renderDataTable(rice_plot)
      }else{
        datamap<-rice_plot[which(rice_plot$ENID==input$enumeratorfinder), ]
        output$table1 <- renderDataTable(rice_plot)
      }
      
    }else {
      if ("All" %in% input$enumeratorfinder){
        datamap<-rice_plot
        output$table1 <- renderDataTable(rice_plot)
      }else{
        datamap<-rice_plot[which(rice_plot$ENID==input$enumeratorfinder), ]
        output$table1 <- renderDataTable(rice_plot)
      }
    }
    
    
    
    leafletProxy("trials_map") %>%
      clearShapes()%>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data=rwa_shp, color="grey",fillOpacity = 0.1,weight = 1.2)%>%
      addPolygons(data=rwad_shp, color="grey",fillOpacity = 0.0,weight = 1.2,label = lapply(labs, HTML))%>%
      addCircles(data = datamap ,lng = ~X_geopoint_longitude, lat = ~X_geopoint_latitude)
    
    ##Summary_submissions trend
    #group by date
    wgroup <- datamap %>%
      mutate(date = as.Date(today)) %>%
      select(date) %>%
      group_by(date) %>%
      count() %>%
      #rename(total_freq = n) %>%
      mutate(date = as.Date(date))
    
    #plot of submissions trend
    Ir<-ggplot(wgroup, aes(x=as.Date(date), y=as.numeric(freq), group=1)) +
      geom_line(color="orange")+
      geom_point(color="orange")+
      #scale_x_discrete(labels= paste("Week", c(1:length(ff))))+
      theme_bw(base_size = 24)+
      labs(title="", x="Date", y="Submissions Count")+scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%m-%Y")+them2
    
    #plotly -interactive ouput
    output$submission_trend<-renderPlotly({
      ggplotly(Ir, tooltip="y")
    })
    
    
    register_hh2<-register_hh[c("HHID", "ENID")]
    
    a <- datamap %>% dplyr:: select("ENID",starts_with("parameters"))
    colnames(a)<-gsub("parameters.", "" , colnames(a) )
    
    
    output$tableR <- renderReactable({
      reactable(a,
                groupBy = c("ENID"),
                columns = list(
                  #plantVigor =colDef(aggregate = "sum")
                  
                  
                ),
                bordered = TRUE
      )
      
    })
    
    output$tableH <- renderReactable({
      reactable(register_hh2,
                groupBy = c("ENID"),
                columns = list(
                  Grain_Yield=colDef(aggregate = "sum")
                ),
                bordered = TRUE
      )
      
    })
    
    
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

# a <- bean_plot %>% dplyr:: select("ENID",starts_with("parameters"))
# colnames(a)<-gsub("parameters.", "" , colnames(a) )
# a <- bean_plot %>% dplyr:: select(starts_with("parameters"))
# colnames(a)<-gsub("parameters.", "" , colnames(a) )
# 
# 
# 
# View(bean_plot)
# View(register_hh)
# a<-bean_plot[ , grepl( "parameters" , names( bean_plot ) ) ]
# a1<-bean_plot[ , gsub( "plot" , names( bean_plot ) ) ]
# 
# library(magrittr)
# 
#   
# a1<-a[ ,gsub("parameters.", "" , names( a ) )]
# 
# colnames(a)<-gsub("parameters.", "" , colnames(a) )
# 
# head(a)
# names(a)
