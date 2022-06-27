#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#    https://mastering-shiny.org/

library(shiny)
library(tidyverse)
library(dbplyr)
# install.package("devtools")
# devtools::install_github("gadenbuie/lorem")
library(lorem)
library(uuid)
library(lubridate)

library(ggplot2)
library(ggthemes)
library(maps)
library(stringi) # used to generate BE#'s
library(ggiraph)
library(igraph)
library(ggraph)
library(patchwork)

# Define UI for application that explores CM data
ui <- fluidPage(
    #theme=bslib::bs_theme(bootswatch="solar"),
    # Application title
    titlePanel("Informaton Collection Explorer"),
    
    # Sidebar with a slider input for number of bins 
    fluidRow(uiOutput("choose_PIR"),
        #selectInput("var",
         #               label = "Select PIR of Interest",
          #              choices = sort(unique(cr_rfc_pir2$NAME)))
        ),
    
    fluidRow(column(12,
        # Show a plot of the generated distribution
        tabsetPanel(
            tabPanel("Table",
                        dataTableOutput("distTable")
                        ),
            tabPanel("Timeline", 
                     plotOutput("distPlot")
                     ),
            tabPanel("Map",
                     plotOutput("mapPlot")
                     ),
            tabPanel("Key Metrics",
                     plotOutput("metrics") 
                     ),
            tabPanel("Network View",
                     plotOutput("network")
            )
        ))
    )
)

# Define server logic 
server <- function(input, output) {
    # load the data
    cr_rfc_pir2 <- readRDS("data/combo.rds")
    # create the network data objects
    # Generate Vertices
    
    vertex_pir <- cr_rfc_pir2 %>% 
        select(NAME, GUID=PIR, CCMD, Classification, Releasability) %>%
        mutate(Priority = as.integer(""), Approval = "", TYPE="PIR", 
               COLOR="gold", CNAME=NAME, PNAME=NAME, shape="circle") %>% 
        distinct()
    vertex_rfc <- cr_rfc_pir2 %>% 
        select(NAME=NAME.rfc, GUID=RFC, CCMD, Classification, Releasability, 
               Priority=Priority.rfc, 
               Approval=Workflow_Status.rfc,
               PNAME=NAME) %>% 
        mutate(Priority = as.integer(Priority), TYPE="RFC", 
               COLOR="tomato", CNAME=NAME, shape="square") %>%
        distinct()
    vertex_cr  <- cr_rfc_pir2 %>% 
        select(NAME=NAME.cr, GUID, CCMD, Classification, Releasability, 
               Priority=Priority.cr, 
               Approval = Workflow_Status.cr,
               PNAME=NAME) %>% 
        mutate(Priority = as.integer(Priority), TYPE="CR", 
               COLOR="gray50",CNAME=NAME, shape="square") %>%
        distinct()
    
    #vertex <- inner_join(vertex_pir, vertex_rfc) 
    
    vertex <- rbind(vertex_pir, vertex_rfc, vertex_cr)
    
    # Generate Edges
    edges_rfc_pir <- cr_rfc_pir2 %>% 
        select(G1=NAME, G2=NAME.rfc, GUID1=PIR, GUID2=RFC) %>% 
        mutate(TYPE1="PIR", TYPE2="RFC", TYPE="RQMT", color="blue", lty="dashed") %>% distinct()
    # mutate(G1=GUID, G2= PIR,TYPE1="PIR", TYPE2="RFC")
    edges_cr_rfc <- cr_rfc_pir2 %>% 
        select(G1=NAME.cr,G2=NAME.rfc, GUID1=GUID, GUID2=RFC) %>% 
        mutate(TYPE1="CR",TYPE2="RFC", TYPE="COLLECT", color="red", lty="dotted") %>% distinct()
    edges <- rbind(edges_rfc_pir,edges_cr_rfc)
    
    net <- graph_from_data_frame(d=edges, vertices=vertex, direct="TRUE")
    net <- delete.vertices(net,degree(net)<1) # remove stand alone nodes
    
    V(net)$label <- V(net)$CNAME
    V(net)$color <- V(net)$COLOR
    V(net)$type  <- V(net)$TYPE
    V(net)$parent <- V(net)$PNAME
    
    
# code for all the displays
    
# pick the PIR
    output$choose_PIR <- renderUI({
        selectInput("var",
                    label = "Select PIR of Interest",
                    choices = sort(unique(cr_rfc_pir2$NAME)))
    })
    
# timeline
    output$distPlot <- renderPlot({
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
        
        g2
        
    })

    #table    
    output$distTable <- renderDataTable({
        t2 <- cr_rfc_pir2 %>% 
            filter(NAME==input$var) %>% 
            mutate(Type=requestType,
                   Request=NAME.rfc,
                   Collection=NAME.cr,
                   Facility=TargetID.rfc,
                   Organization=Combatant_Command.rfc) %>%
            select(Type,
                   Request,
                   Collection,
                   Facility,
                   Organization)
        t2 
    }, options=list(pageLength=15, info=FALSE))
    
    #map
    output$mapPlot <- renderPlot({
        usa <- map_data("world")
        m2 <- cr_rfc_pir2 %>% filter(NAME==input$var)
        ggplot() + 
            geom_map(data=usa,map=usa, aes(x=long,y=lat,map_id=region), fill="orange",color="black") +
            geom_point(data=m2, aes(x=lon.rfc,y=lat.rfc), color="red", size=4) +
            geom_point(data=m2, aes(x=lon.cr,y=lat.cr), color="blue", size=1)
    })
    
    output$metrics <- renderPlot({
        thematic::thematic_shiny()

        
        p1 <- ggplot(cr_rfc_pir2) + geom_bar(aes(x=.data$Priority.cr))
        p2 <- ggplot(cr_rfc_pir2) + geom_bar(aes(x=.data$Combatant_Command.cr)) 
        p3 <- ggplot(cr_rfc_pir2) + geom_bar(aes(x=.data$Workflow_Status.cr))
        p4 <- ggplot(cr_rfc_pir2) + geom_bar(aes(x=.data$requestType))
        
        
        cr_rfc_pir3 <- cr_rfc_pir2 %>% filter(.data$NAME == .env$input$var)
        p1 <- ggplot(cr_rfc_pir3) + geom_bar(aes(x=.data$Priority.cr, fill=Priority.cr))
        #p2 <- ggplot(cr_rfc_pir3) + geom_bar(aes(x=.data$Combatant_Command.cr)) 
        p3 <- ggplot(cr_rfc_pir3) + geom_bar(aes(x=.data$Workflow_Status.cr))
        p4 <- ggplot(cr_rfc_pir3) + geom_bar(aes(x=.data$requestType))
        
        
        g4 <- p3 / ( p1 | p4 )
        return(g4)

    })
    
    output$network <- renderPlot({
        thematic::thematic_shiny()
        net2 <- delete.vertices(net,V(net)$parent!=input$var)

        
        ggraph(net2, layout="fr") + 
            geom_edge_link(aes(color=TYPE)) + 
            geom_node_point(aes(color=type, size=5)) +
            geom_node_text(aes(label=label), repel=TRUE)
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
