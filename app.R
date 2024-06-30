library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(DT)

source("prepo n viz.R")
datak <- read.csv("Hotel Reservations.csv")
datak <- cbind(datak[, 1:11], date = as.Date(paste(datak$arrival_date, datak$arrival_month, datak$arrival_year, sep="-"), format="%d-%m-%Y"), datak[, 12:ncol(datak)])
##mengecek data null pada kolom date
subset(datak,is.na(date),c(arrival_year,arrival_month, arrival_date, date))
##data 29 Februari 2018 tidak ada pada kalendar, maka data dihapus dari original dataset
datak <- datak[complete.cases(datak$date), ]
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
          box(dateRangeInput("dateRange","Filter by Date",start = min(data$date),end = max(data$date))),
          box(title = strong("Distribution of Canceled Bookings"), solidHeader = T,
              width = 12, 
              plotOutput("tgplot1", height = "400px")),
          box(title = strong("Book Count each day"), solidHeader = T,
              width = 12, 
              plotOutput("tgplot2", height = "300px")),
          box(title = strong("Average Price per Room"), solidHeader = T,
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
          box(dateRangeInput("dateRange2","Filter by Date",start = min(datak$date),end = max(datak$date))),
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
                 )
        )
    ))),
    tabItem(
      tabName = "predict",
      fluidRow(
        width = 4,
        box(width =4,
          numericInput("adult","No of Adult", value = NULL),
          numericInput("childern","No of Childern", value = NULL),
          numericInput("weekend","No of Weekend Nigths", value = NULL),
          numericInput("week","No of Week Nigths", value = NULL),
          selectInput("mealplan","Type of Meal Plan",c(0,1,2,3), selected = NULL),
          selectInput("parking","Required Car Parking Space",c(0,1), selected = NULL),
          selectInput("roomtype","Room Type Reserved",c(1,2,3,4,5,6,7), selected = NULL),
          numericInput("leadtime","Lead Time", value = NULL),
          dateInput("date","Arrival Date",value=NULL,format = "dd-mm-yyyy",startview = "month"),
          selectInput("marketseg","Market Segment Type",c("Aviation","Complementary","Corporate","Offline","Online"), selected = NULL),
          selectInput("repguest","Repeated Guest",c(0,1), selected = NULL),
          numericInput("prev_cancel","No of Previous Cancellations", value = NULL),
          numericInput("prev_nocancel","No of Previous Bookings not Canceled", value = NULL),
          numericInput("price","Average Price per Room", value = NULL),
          numericInput("request","No of Special Request", value = NULL),
          actionButton("pred", "Predict")
        )
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
  fdata <- reactive({
    data %>%
      filter(date >= input$dateRange[1] & date <= input$dateRange[2])})
  fdatak <- reactive({
    datak %>%
      filter(date >= input$dateRange2[1] & date <= input$dateRange2[2])})
  output$tgplot1 <- renderPlot(
    tgplot1(fdata())
  )
  output$tgplot2 <- renderPlot(
    tgplot2(fdata())
  )
  output$tgplot3 <- renderPlot(
    tgplot3(fdata())
  )
  output$tgplot4 <- renderPlot(
    tgplot4(fdatak())
  )
  output$tgplot5 <- renderPlot(
    tgplot5(fdatak())
  )
  output$tgplot6 <- renderPlot(
    tgplot6(fdatak())
  )
  output$tgplot7 <- renderPlot(
    tgplot7(fdata())
  )
  output$tgplot8 <- renderPlot(
    tgplot8(fdata())
  )
  output$dat <- renderDT(datak)
 
}
shinyApp(ui=ui,server=server,
         options = list(launch.browser=TRUE))