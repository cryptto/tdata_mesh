#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#    https://mastering-shiny.org/

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme=bslib::bs_theme(bootswatch="yeti"),
    # Application title
    titlePanel("PIR Explorer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # sliderInput("bins",
            #             "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30),
            selectInput("var",
                        label = "PIRs",
                        choices = ordered(unique(rfc_pir2$NAME.pir)))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot", height="600px"),
           dataTableOutput("distTable"),
           plotOutput("mapPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
       # x    <- faithful[, 2]
       # bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
      #  hist(x, breaks = bins, col = 'darkgray', border = 'white')
        thematic::thematic_shiny()
        
        g2 <- cr_rfc_pir2 %>% filter(NAME==input$var) %>%# filter(PIR == PIR_OI) %>%
            mutate(MidPoint = collectionStartDate.cr +       int_length(interval(collectionStartDate.cr,collectionStopDate.cr))/2 ) %>% # select(MidPoint)
            arrange(GUID) %>%
            ggplot(aes(y=NAME.cr)) +
            geom_segment_interactive(aes(x=collectionStartDate.cr, 
                             xend=collectionStopDate.cr, 
                             y=NAME.cr, 
                             yend=NAME.cr,
                             size=1.5, 
                             color=NAME.rfc,
                             alpha=Workflow_Status.cr, tooltip=GUID), 
                         lineend="round") + 
            geom_label( aes( label=str_wrap(NAME.cr, 12), 
                             x=MidPoint ), 
                        size=3.5) +
            facet_wrap(~NAME.rfc , scales="free_y", ncol=1) # + my_theme
        
       # g2
        
        g3 <- 
          cr_rfc_pir2 %>% filter(.data$PIR == .env$input$var) %>%
          mutate(Combatant_Command.rfc = as.factor(Combatant_Command.rfc)) %>%
          mutate(Priority.rfc = as.factor(Priority.rfc)) %>%
          mutate(Combatant_Command.cr = as.factor(Combatant_Command.cr)) %>%
          mutate(Priority.cr = as.factor(Priority.cr)) %>%
          mutate(requestType = as.factor(requestType)) %>% 
          mutate(Workflow_Status.rfc = as.factor(Workflow_Status.rfc)) %>%
          mutate(Workflow_Status.cr = as.factor(Workflow_Status.cr))
        
        
        p1 <- ggplot(g3) + geom_bar(aes(x=.data$Priority.cr))
        # p1
        hist(table(g3$Priority.rfc))
        
        
    })
    
    output$distTable <- renderDataTable({
        t2 <- cr_rfc_pir2 %>% filter(NAME==input$var) %>% select(NAME.cr,TYPE.cr,NAME.rfc,TYPE.rfc)
        t2 
    }, options=list(pageLength=5, info=FALSE))
    
    output$mapPlot <- renderPlot({
        usa <- map_data("usa")
        m2 <- cr_rfc_pir2 %>% filter(NAME==input$var)
        ggplot() + 
            geom_map(data=usa,map=usa, aes(x=long,y=lat,map_id=region), fill="orange",color="black") +
            geom_point(data=m2, aes(x=lon.rfc,y=lat.rfc), color="red", size=4) +
            geom_point(data=m2, aes(x=lon.cr,y=lat.cr), color="blue", size=1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
