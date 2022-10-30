library(shiny)
library(RMySQL)
library(DBI)
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
                 dbname = 'semester', host = 'localhost')
ui <- fluidPage(
  numericInput('pincode', 'location_name','district', value = 1L, step = 1L),
  textInput('col1', 'col2', 'col3', value = 'a'),
  actionButton('writetodb', 'Save'),
  tableOutput('dbtable')
)
server <- function(input, output) {
  mytableinshiny <- reactive({
    dbtrigger$depend()
    dbGetQuery(con, 'SELECT pincode, location_name, district from location')
  })
  observeEvent(input$writetodb, {
    sql1 = "INSERT INTO location (pincode,location_name,district) VALUES (?pincode, ?location_name,?district)"
    sql <- sqlInterpolate(con, sql1, col1=input$pincode, col2=input$location_name, col3=input$district)
    dbExecute(con, sql)
    dbtrigger$trigger()
  })
  output$dbtable <- renderTable({
    mytableinshiny()
  })
}
shinyApp(ui , server )

