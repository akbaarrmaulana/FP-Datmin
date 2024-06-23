library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)

# Header
headerItem <- dashboardHeader(title = " ")
# Sidebar
sidebarItem <- dashboardSidebar(
  sidebarMenu(
    menuItem("EDA", tabName = "EDA", icon = icon("magnifying-glass-chart")),
    menuItem("Dataset", tabName = "EDA", icon = icon("hotel")),
    menuItem("Prediction", tabName = "EDA", icon = icon("tree"))
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

                                    ")),
  tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Dashboard Pariwisata dan Ekonomi Kreatif </span>\');
      })
     ')),
  shinyDashboardThemes("purple_gradient"),
  tabItems(
    tabItem(tabName = "EDA",
      titlePanel(
        h1(strong("Hotel Reservations EDA"),
           style="text-align:center;",style = "margin-bottom:-20px;",style = "margin-top:-20px;")
      ),
      tabsetPanel(
        tabPanel("A"),
        tabPanel("B")
      )
    )
  )
)

ui <- dashboardPage(
  header = headerItem,
  sidebar = sidebarItem,
  body = bodyItem
)
server <- function(input,output,session){
  
}
shinyApp(ui=ui,server=server,
         options = list(launch.browser=TRUE))