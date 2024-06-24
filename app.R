library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(DT)

source("prepo n viz.R")
datak <- read.csv("Hotel Reservations.csv")
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
          "Each Variable",
          box(title = strong("Distribution of Canceled Bookings"), solidHeader = T,
              width = 12, 
              plotOutput("tgplot1", height = "300px")),
          box(title = strong("Book Count each day"), solidHeader = T,
              width = 12, 
              plotOutput("tgplot2", height = "300px")),
          box(title = strong("Average Price per Room (2017-2018)"), solidHeader = T,
              width = 12, 
              plotOutput("tgplot3", height = "300px")),
          box(title = strong("Distribution of Number of Week and Weekend Nights"), solidHeader = T,
              width = 12, 
              plotOutput("tgplot7", height = "300px")),
          box(title = strong("Distribution of the Number of Adults and Children"), solidHeader = T,
              width = 12, 
              plotOutput("tgplot8", height = "300px"))
        ),
        tabPanel(
          "Group By",
          box(title = strong("Distribution of Meal Plan Types by Cancellation Status"), solidHeader = T,
              width = 12, 
              plotOutput("tgplot4", height = "300px")),
          box(title = strong("Distribution of Room Types Reserved by Cancellation Status"), solidHeader = T,
              width = 12, 
              plotOutput("tgplot5", height = "300px")),
          box(title = strong("Distribution of Market Segments by Cancellation Status"), solidHeader = T,
              width = 12, 
              plotOutput("tgplot6", height = "300px"))
          )
      )
    ),
    tabItem(
      tabName = "data",
      titlePanel(
        h1(strong("Hotel Reservations Dataset"),
           style="text-align:center;",style = "margin-bottom:-20px;",style = "margin-top:-20px;")
      ),
      tabsetPanel(
        tabPanel("Dataset",
                 DTOutput("dat")),
        tabPanel("Info",
                 fluidRow(
                   width = 12,
                   box(
                     title = "Data Dictionary",
                     solidHeader = TRUE,
                     width = 12,
                     HTML(
                       "<ul>
                        <li><b>Booking_ID:</b> Unique identifier of each booking</li>
                        <li><b>no_of_adults:</b> Number of adults</li>
                        <li><b>no_of_children:</b> Number of children</li>
                        <li><b>no_of_weekend_nights:</b> Number of weekend nights (Saturday or Sunday) the guest stayed or booked to stay at the hotel</li>
                        <li><b>no_of_week_nights:</b> Number of week nights (Monday to Friday) the guest stayed or booked to stay at the hotel</li>
                        <li><b>type_of_meal_plan:</b> Type of meal plan booked by the customer</li>
                        <li><b>required_car_parking_space:</b> Does the customer require a car parking space? (0 - No, 1 - Yes)</li>
                        <li><b>room_type_reserved:</b> Type of room reserved by the customer. The values are ciphered (encoded) by INN Hotels</li>
                        <li><b>lead_time:</b> Number of days between the date of booking and the arrival date</li>
                        <li><b>arrival_year:</b> Year of arrival date</li>
                        <li><b>arrival_month:</b> Month of arrival date</li>
                        <li><b>arrival_date:</b> Date of the month</li>
                        <li><b>market_segment_type:</b> Market segment designation</li>
                        <li><b>repeated_guest:</b> Is the customer a repeated guest? (0 - No, 1 - Yes)</li>
                        <li><b>no_of_previous_cancellations:</b> Number of previous bookings that were canceled by the customer prior to the current booking</li>
                        <li><b>no_of_previous_bookings_not_canceled:</b> Number of previous bookings not canceled by the customer prior to the current booking</li>
                        <li><b>avg_price_per_room:</b> Average price per day of the reservation; prices of the rooms are dynamic (in euros)</li>
                        <li><b>no_of_special_requests:</b> Total number of special requests made by the customer (e.g., high floor, view from the room, etc)</li>
                        <li><b>booking_status:</b> Flag indicating if the booking was canceled or not</li>
                      </ul>"
                   )
                 ))
        )
    ),
    tabItem(
      tabName = "predict"
    )
  )
))

ui <- dashboardPage(
  header = headerItem,
  sidebar = sidebarItem,
  body = bodyItem
)
server <- function(input,output,session){
  output$tgplot1 <- renderPlot(
    tgplot1(data)
  )
  output$tgplot2 <- renderPlot(
    tgplot2(data)
  )
  output$tgplot3 <- renderPlot(
    tgplot3(data)
  )
  output$tgplot4 <- renderPlot(
    tgplot4(data)
  )
  output$tgplot5 <- renderPlot(
    tgplot5(data)
  )
  output$tgplot6 <- renderPlot(
    tgplot6(data)
  )
  output$tgplot7 <- renderPlot(
    tgplot7(data)
  )
  output$tgplot8 <- renderPlot(
    tgplot8(data)
  )
  output$dat <- renderDT(datak)
 
}
shinyApp(ui=ui,server=server,
         options = list(launch.browser=TRUE))