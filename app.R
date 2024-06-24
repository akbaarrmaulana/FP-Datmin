library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)

source("prepo n viz.R")
data <- readRDS("dataclean.rds")

# Header
headerItem <- dashboardHeader(title = " ")
# Sidebar
sidebarItem <- dashboardSidebar(
  sidebarMenu(
    menuItem("EDA", tabName = "EDA", icon = icon("magnifying-glass-chart")),
    menuItem("Dataset", tabName = "data", icon = icon("hotel")),
    menuItem("Prediction", tabName = "predict", icon = icon("tree"))
  )
)

bodyItem <- dashboardBody(
  tags$style(HTML("


.box.box-solid.box-primary>.box-header {
  color:#fff;
  background:#c7c6c1
                    }

.box.box-solid.box-primary{
border-bottom-color:#fff;
border-left-color:#fff;
border-right-color:#fff;
border-top-color:#fff;
}

.box.box-solid.box-success>.box-header {
  color:#fff;
  background:#5f9ea0
}
.box.box-solid.box-success{
border-bottom-color:#fff;
border-left-color:#fff;
border-right-color:#fff;
border-top-color:#fff;
}

.main-header .logo {
  font-family: helvetica,  serif, Times, Times New Roman;
  font-weight: bold;
  font-size: 24px;
}
.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: helvetica,  serif, Times, Times New Roman;
        padding: 0 15px;
        overflow: hidden;
        color: white;
}
.box {
  background-color: white !important;
}      

                                    ")),
  tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Hotel Reservations Dashboard </span>\');
      })
     ')),
  shinyDashboardThemes("onenote"),
  tabItems(
    tabItem(tabName = "EDA",
      titlePanel(
        h1(strong("Hotel Reservations EDA"),
           style="text-align:center;",style = "margin-bottom:-20px;",style = "margin-top:-20px;")
      ),
      tabsetPanel(
        tabPanel(
          "Target",
          box(title = strong("Distribution of Canceled Bookings"), solidHeader = T,
              width = 7, 
              plotOutput("tgplot1", height = "300px"))
        ),
        tabPanel("B")
      )
    ),
    tabItem(
      tabName = "data",
      titlePanel(
        h1(strong("Hotel Reservations Dataset"),
           style="text-align:center;",style = "margin-bottom:-20px;",style = "margin-top:-20px;")
      ),
      tabsetPanel(
        tabPanel("Dataset"),
        tabPanel("Info")
        )
    ),
    tabItem(
      tabName = "predict"
    )
  )
)

ui <- dashboardPage(
  header = headerItem,
  sidebar = sidebarItem,
  body = bodyItem
)
server <- function(input,output,session){
  output$tgplot1 <- renderPlot(
    tgplot1(data)
  )
}
shinyApp(ui=ui,server=server,
         options = list(launch.browser=TRUE))