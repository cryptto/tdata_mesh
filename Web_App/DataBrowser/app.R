#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(DBI)
library(RMariaDB)
library(tidyverse)
library(dbplyr)
library(reactable)

# establish database connection
con <- dbConnect(RMariaDB::MariaDB(), dbname="default_1", user="coker", password="coker", host="localhost", group="my-db")
# get list of available tables
tlist <- dbListTables(con)
# get list of fields in each of these tables
flist <- dbListFields(con,"pir")
# execute sample query to return a single record
rs_string <- paste0("SELECT * FROM ",tlist[3]," LIMIT 1")
# rs <- dbSendQuery(con, "SELECT * FROM pir LIMIT 1")
rs <- dbSendQuery(con,rs_string)
# get information about fields and type for each column
filist <- dbColumnInfo(rs)
dbDisconnect(con)

library(shinydashboard)
library(shiny)


ui <- dashboardPage(
    dashboardHeader(title = "Data Explorer"),
    dashboardSidebar(
        uiOutput("choose_data"),
        uiOutput("db_cols")
        ),
    dashboardBody(
        valueBox(class(con), "Basic example"),
        #reactableOutput("mtcars"),
        reactableOutput("db_subset_table")
    )
)


server <- function(input, output) {
    
    ds_list <- reactive ({
        con <- dbConnect(RMariaDB::MariaDB(), dbname="default_1", user="coker", password="coker", host="localhost", group="my-db")
        df <- dbListTables(con) 
        dbDisconnect(con)
        df
    })
    
    ds_cols <- reactive({
        con <- dbConnect(RMariaDB::MariaDB(), dbname="default_1", user="coker", password="coker", host="localhost", group="my-db")
        df <- dbListFields(con, input$data_choice) 
        dbDisconnect(con)
        df
    })
    
    ds_all <- reactive({
        con <- dbConnect(RMariaDB::MariaDB(), dbname="default_1", user="coker", password="coker", host="localhost", group="my-db")

        if(is.null( input$data_choice ) ) {
           df <- tbl(con,"pir") %>% collect()
        }
        else { df <- tbl(con, input$data_choice) %>% collect() }
        dbDisconnect(con)
        df
    })
    
    ds_subset <- reactive({
        con <- dbConnect(RMariaDB::MariaDB(), dbname="default_1", user="coker", password="coker", host="localhost", group="my-db")

        if(is.null( input$db_cols ) ) {
            df <- tbl(con,"pir") %>% collect() 
        }
        else { df <- tbl(con, input$data_choice) %>% collect()  %>% select(c(input$db_cols) )}
        dbDisconnect(con)
        df
    })
    
    
  ########################## Functional Elements ##############  
    output$mtcars <- renderReactable( {
        reactable(mtcars)
    })
    
    output$db_table <- renderReactable( {
        reactable( ds_all() )
    })
    
    output$db_subset_table <- renderReactable( {
        reactable( ds_subset() )
    })
    
    output$choose_data <- renderUI({
        selectInput("data_choice",
                    label = "Select a data set of Interest",
                    choices = sort(unique( ds_list() )))
    })
    
    output$db_cols <- 
        renderUI({
            selectInput("col_choice",
                        label = "Select Columns of Interest",
                        choices = sort(unique( ds_cols() )),
                        multiple=TRUE, size = 10, selectize = FALSE
                        )
        })
        
}





shinyApp(ui, server)