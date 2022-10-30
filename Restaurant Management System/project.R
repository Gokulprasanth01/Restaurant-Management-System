library(shiny)
library(RMySQL)
library(DBI)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readxl)
library(janitor)
library(lubridate)
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)
library(ggthemes)
library(shinythemes)
library(htmlwidgets)
library(viridis)
library(waiter)
library(highcharter) 
library(DT)
library(RSQLite)
library(shinyFeedback)    
library(qdapRegex)
library(imager)
library(shinymanager)
library(pool)
library(rhandsontable)
library(shinyalert)
library(officer)
library(glue)
library(sortable)
library(flextable)
library(shinyTime)
library(ggplot2)
library(flexdashboard)
library(readr)
library(timevis)
library(patchwork)
library(shinycssloaders)
library(gmailr)
makereactivetrigger <- function() {
  rv <- reactiveValues(a = 0)
  list(
    depend = function() {
      rv$a
      invisible()
    },
    trigger = function() {
      rv$a <- isolate(rv$a + 1)
    }
  )
}

dbtrigger <- makereactivetrigger()
con <- dbConnect(MySQL(), user = 'root', password = 'Gokul@0127',
                 dbname = 'pro', host = 'localhost')

ui<-dashboardPage(
  
  dashboardHeader(title="RADISON BLUE RESTAURANT "),
  dashboardSidebar("          ",
                   width=150,
                   sidebarMenu(      
                     menuItem(h4("Home") , tabName = "Home", icon = icon("house-user")),
                     menuItem(h4("Menu-Card") , tabName = "Menu_Card", icon = icon("bars")),
                     menuItem(h4("Location"), tabName = "Location", icon = icon("map")),
                     menuItem(h4("Combo-Offers"), tabName = "Combo_offers", icon = icon("bahai")),
                     menuItem(h4("Orders"), tabName = "Orders", icon = icon("phone")),
                     menuItem(h4("Review"), tabName = "Review", icon = icon("star")),
                     menuItem(h4("Help & Support"), tabName = "Help", icon = icon("phone"))
                     
                     
                     
                   )),
  dashboardBody(
    
    setBackgroundColor(
      color = c("#F7FBFF", "#2171B5"),
      gradient = "linear",
      direction = "bottom"),
    
    setBackgroundImage(src = "http://wallpics4k.com/wp-content/uploads/2014/07/470318.jpg"),
    
    
    tabItems(
      tabItem("Home",
              h1("Welcome to our website"),
              h2("Radison Blue Restaurant Best quality and taste"),
              h3("PLAY WITH YOUR FOOD!!!!!")
      ),
      tabItem("menu_Card",
              fluidPage(
                div(p(h1("Taste good"))),
                div(p(h2("Select your dish"))),
                div(p(h2("Starters"))),
                div(p(actionLink("do", h3("Malaai-Cutlad------------Rs.80"),width=300))),
                div(p(actionLink("do", h3("Baby Corn Fries----------------Rs.190"),width=300))),
                div(p(actionLink("do", h3("Chicken 65----------Rs.150(8pcs)"),width=300))),
                div(p(actionLink("do", h3("Dragon Chicken-----------Rs.200"),width=300))),
                div(p(actionLink("do", h3("Chilli Chicken-------------Rs.250(8Pcs)"),width=300))),
                div(p(actionLink("do", h3("Triple 7-----------Rs.250"),width=300))),
                div(p(actionLink("do", h3("Mutton Chopps----------Rs.100"),width=300))),
                div(p(actionLink("do", h3("Prawn 65------------------Rs.120"),width=300))),
                div(p(actionLink("do", h3("Fish Fingers------------------Rs.220"),width=300))),
                div(p(h2("Main Course"))),
                div(p(actionLink("do", h3("Porucha Parota----------Rs.150(2Pcs)"),width=300))),
                div(p(actionLink("do", h3("Full Grill------------------Rs.320"),width=300))),
                div(p(actionLink("do", h3("Half Grill------------------Rs.160"),width=300))),
                div(p(actionLink("do", h3("Bun Parota------------------Rs.120"),width=300))),
                div(p(actionLink("do", h3("Chicken Parota------------------Rs.160"),width=300))),
                div(p(actionLink("do", h3("Chilli Parota------------------Rs.130"),width=300))),
                div(p(actionLink("do", h3("Egg Briyani------------------Rs.100"),width=300))),
                div(p(actionLink("do", h3("Mutton Briyani------------------Rs.250"),width=300))),
                div(p(actionLink("do", h3("Chicken Briyani------------------Rs.200"),width=300))),
                div(p(actionLink("do", h3("Chicken Noodels------------------Rs.120"),width=300))),
                div(p(actionLink("do", h3("Chicken Fried Rice------------------Rs.120"),width=300))),
                div(p(h2("Desserts"))),
                div(p(actionLink("do", h3("Faluda------------------Rs.100"),width=300))),
                div(p(actionLink("do", h3("Brownie Pastry------------------Rs.120"),width=300))),
                div(p(actionLink("do", h3("Nutella Brownie------------------Rs.108"),width=300))),
                div(p(actionLink("do", h3("Choco Lava Cake------------------Rs.150"),width=300))),
                div(p(actionLink("do", h3("Double Trouble------------------Rs.125"),width=300))),
              )
              
      ),
      
      
      tabItem("location",
              
              fluidPage(
                numericInput('pincode','Pincode', value = 1L, step = 1L),
                textInput('location_name','Location_name'),
                textInput('district','District'),
                actionButton('writetoloc', 'Save'),
                
              ),
      ),
      
      tabItem("combo_offers",
              fluidPage(
                h1("Today Special"),
                h1("Pick your Combo here!!!!"),
                dataTableOutput("Offer"),
                div(p(actionLink("do", h4("Chicken Biriyani And Mutton Chops ------------- Rs.750"),width=300))),
                div(p(actionLink("do", h4("Shawarma and Hariyali Kebab ------------------- Rs.350"),width=300))),
                div(p(actionLink("do", h4("Mutton Biriyani And Chilli Chicken ------------ Rs.450"),width=300))),
                div(p(actionLink("do", h4("Porucha Parotta And Malaii Cutlet ------------- Rs.250"),width=300))),
                
              ),
      ),
      tabItem("orders",
              fluidPage(
                h1("Place your Order here!!!!!!"),
                numericInput('order_id','Order_id', value = 1L, step = 1L),
                textInput('Order_dish','Order_dish'),
                numericInput('quantity','Quantity', value = 1L, step = 1L),
                actionButton('writetoa', 'Save')
              ),
      ),
      tabItem("review",
              fluidPage(
                h1("Throw your Review here!!!!!"),
                textInput('name','Name'),
                textInput('comment','Comment'),
                actionButton('writetodb','save'),
              ),
      ),
      tabItem("help",
              fluidPage(
                
                h1("Help Line:"),
                h3("+91 8942521422"),
                h3("ngk0127@gmail.com"),
                h3(" 1)	         Police Control Room---------dial--100"),
                h3(" 2)	       Fire & Rescue Services----------dial--101"),
                h3(" 3)	             Ambulance-----------dial--108"),
                h3(icon = icon(""))
                
              ))
      
    )
  ))


server <- function(input, output) {
  observeEvent(input$writetodb, {
    sql2 = "INSERT INTO review VALUES (?name,?comment)"
    sql3 <- sqlInterpolate(con, sql2, name=input$name, comment=input$comment)
    dbExecute(con, sql3)
    dbtrigger$trigger()
  })
  observeEvent(input$writetoloc, {
    sql1 = "INSERT INTO location VALUES (?pincode,?location_name,?district)"
    sql <- sqlInterpolate(con, sql1, pincode=input$pincode, location_name=input$location_name, district=input$district)
    dbExecute(con, sql)
    dbtrigger$trigger()
  })
  observeEvent(input$writetoa, {
    sql4 = "insert into order1 values (?order_id,?Order_dish,?quantity)"
    sql5 <- sqlInterpolate(con, sql4, order_id=input$order_id,Order_dish=input$Order_dish,quantity=input$quantity)
    dbExecute(con, sql5)
    dbtrigger$trigger()
  })
  
}

shinyApp(ui, server)
