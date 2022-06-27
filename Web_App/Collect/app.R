#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#    https://mastering-shiny.org/

library(shiny)
library(bslib)
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
library(leaflet)
library(terra)
library(plotly)
library(reactable)

# Define UI for application that explores CM data
ui <- navbarPage(windowTitle="Information Collection Explorer",
                 title=div(
                          img(src="Gakoi.png",height=40,width=40,
                              style="margin:5px 5px"),
                          "Information Collection Explorer"),
                           
    #icon="Gakoi.png",
    theme = bs_theme(version = 4, bootswatch = "sandstone"),
    tabPanel("PIR View",
             sidebarLayout(
                 sidebarPanel(uiOutput("choose_PIR"),
                              width = 2),
                 mainPanel(fluidRow(column(
                     10,
                     tabsetPanel(
                         #tabPanel("Table",
                          #        dataTableOutput("distTable")),
                         tabPanel("Fancy Table",
                                  reactableOutput("reacTable")),
                         tabPanel("Timeline",
                                  plotOutput("timelinePlot"),height="100%"),
                         tabPanel("Map",
                                  plotOutput("mapPlot")),
                         tabPanel("Key Metrics",
                                  plotOutput("metrics")),
                         tabPanel("Network View",
                                  plotOutput("network"))
                     )
                 )))
             )),
    tabPanel("Global View",
             fluidRow(column(
                 10,
                 tabsetPanel(
                     tabPanel("All Records",
                              reactableOutput("AllreacTable")),
                     tabPanel("Global Map",
                              leafletOutput("mapLeaflet")),
                     tabPanel("Treemap",
                              plotlyOutput("treeMap"))
                 )
             ))),
    tabPanel("About",
    fluidRow(column(
        10,
        tabsetPanel(
            tabPanel("Introduction",
                     img(src="presentationtitle.png",width="90%"),
                     h4("https://gakoi.shinyapps.io/CollectLeaf/")
                     ),
            tabPanel("Author",
                     h2("David A. Coker, PhD"),
                     h3("davidacoker@gmail.com"),
                     h3("Distinguished Technologist at Booz Allen Hamilton"),
                     h3("R User Since 2014"),
                     h3("Shiny User Since November 2021"),
                     img(src="author.png")),
            tabPanel("Problem Summary",
                     img(src="disaster.png"),
                     h3("Problem"),
                     p("Combine 25+ disparate and inconsistent data sources to coordinate a humanitarian relief effort across multiple organizations."),
                     h3("Technical Approach"),
                     p("Employ a virtual query layer to provide a flexible and responsive approach to create the base data objects supporting a number of disparate user facing applications."),
                     h3("Challenge"),
                     p("How do you quickly demonstrate the value of virtual data to solve this problem?"),
                     h3("Solution"),
                     p("Leverage R/Shiny to quickly stand up an interactive application demonstrating the value of a virtual data layer.")),
            tabPanel("Data Landscape",
                     img(src="datalandscape.png",width="90%")),
            tabPanel("Virtual Data Layer",
                     img(src="virtualdatalayer.png",width="90%")),
            tabPanel("Tech Stack",
                     img(src="techstack.png",width="90%")),
            tabPanel("Observations",
                     h3("Observation 1"),
                     p("Building the initial R/Shiny app only took ~4 hours for someone new to the technology."),
                     h3("Observation 2"),
                     p("Virtual Data Layers make it easy to architect cross-database modeling, views, and operations at scale."),
                     h3("Observation 3"),
                     p("Virtual Data Layers are a natural complement to dplyr providing both flexibility and performance."),
                     h4("https://gakoi.shinyapps.io/CollectLeaf/")
                     )

        )
    )))
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
    
    # https://plotly.com/r/treemaps/
    # code for creating treemap data objects
    ids<- c(cr_rfc_pir2$NAME.cr)
    labels=c(cr_rfc_pir2$NAME.rfc)
    parents=c(cr_rfc_pir2$NAME)
    dddf <- data.frame(ids,labels,parents)
    ddd <- data.frame(ids=unique(cr_rfc_pir2$NAME),
                      labels=unique(cr_rfc_pir2$NAME),
                      parents=NA)
    dddf <- rbind(ddd,dddf)

    # create reactive data objects
    pir_data <- reactive({
        cr_rfc_pir2 %>% 
        # filter(NAME=="PIR14")  %>% #input$var) %>% 
        mutate(Type=requestType,
               Request=NAME.rfc,
               Collection=NAME.cr,
               Facility=TargetID.rfc,
               Requirement=NAME,
               Organization=Combatant_Command.rfc) # %>%
        # select(Type,
        #        Request,
        #        Collection,
        #        Facility,
        #        Organization)
    })
    
    pir_data_filtered <- reactive({
        cr_rfc_pir2 %>%
        filter(NAME==input$var) %>% 
        mutate(Type=requestType,
               Request=NAME.rfc,
               Collection=NAME.cr,
               Facility=TargetID.rfc,
               Requirement=NAME,
               Organization=Combatant_Command.rfc) 
    })

    
    # code for all the displays
    
    # pick the PIR
    output$choose_PIR <- renderUI({
        selectInput("var",
                    label = "Select PIR of Interest",
                    choices = sort(unique(cr_rfc_pir2$NAME)))
    })
    
    # timeline
    output$timelinePlot <- renderPlot({
        #thematic::thematic_shiny()
        
        g2 <- cr_rfc_pir2 %>% filter(NAME==input$var) %>%# filter(PIR == PIR_OI) %>%
            mutate(MidPoint = collectionStartDate.cr +       
                       int_length(interval(collectionStartDate.cr,collectionStopDate.cr))/2 ) %>% # select(MidPoint)
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
                        size=3.5) + theme_few() + theme(legend.position="none") +
            facet_wrap(~NAME.rfc , scales="free_y", ncol=1) # + my_theme
        
        g2
        
    })
    
    #table    
    output$distTable <- renderDataTable({
        pir_data_filtered()
    }, options=list(pageLength=15, info=FALSE))
    
    # fancy table
    output$reacTable <- renderReactable({
        t2 <- pir_data_filtered () %>% 
            select(Type,
                   Request,
                   Collection,
                   Facility,
                   Organization)
        reactable(t2, filterable=FALSE)
    })
    
    # fancy table w/ all data
    output$AllreacTable <- renderReactable({
        t2 <- pir_data() %>% 
            select(Organization, Requirement, Type,
                   Request,
                   Collection,
                   Facility,
            )
        reactable(t2, groupBy=c("Requirement","Type"))
    })
    
    #map
    output$mapPlot <- renderPlot({
        usa <- map_data("world")
        m2 <- cr_rfc_pir2 %>% filter(NAME==input$var)
        ggplot() + theme_gdocs() +
            geom_map(data=usa,map=usa, aes(x=long,y=lat,map_id=region), fill="gray50",color="gray50") +
            geom_point(data=m2, aes(x=lon.rfc,y=lat.rfc), color="red", size=6, alpha=0.1) +
            geom_point(data=m2, aes(x=lon.cr,y=lat.cr), color="blue", size=2, alpha=0.5) # +

    })
    
    # plotly treemap
    output$treeMap <- renderPlotly({
        fig <- plot_ly(type="treemap",
                       ids=dddf$ids,
                       labels=dddf$labels,
                       parents=dddf$parents)
        
    })
    
    # leaflet map of all CRs
    output$mapLeaflet <- renderLeaflet({
        m2<-cr_rfc_pir2
        m <- leaflet() %>%
            addTiles() %>%
            addMarkers(clusterOptions=TRUE,
                       lng=m2$lon.cr, 
                       lat=m2$lat.cr,
                       popup=paste0(m2$NAME,"<br>",m2$NAME.rfc,"<br>",m2$NAME.cr) )
        m
    })
    
    # metrics   
    output$metrics <- renderPlot({
        p1 <- ggplot(pir_data_filtered()) + 
            geom_bar(aes(x=.data$Priority.cr, fill="steelblue")) + 
            theme_gdocs() + guides(fill="none")
        #p2 <- ggplot(cr_rfc_pir3) + geom_bar(aes(x=.data$Combatant_Command.cr)) 
        p3 <- ggplot(pir_data_filtered()) + 
            geom_bar(aes(x=.data$Workflow_Status.cr, fill="gold")) +
            theme_gdocs() + guides(fill="none")
        p4 <- ggplot(pir_data_filtered()) + 
            geom_bar(aes(x=.data$requestType, fill="green")) +
            theme_gdocs() + guides(fill="none")
        
        g4 <- p3 / ( p1 | p4 )
        return(g4)
    })
    
    output$network <- renderPlot({
        #thematic::thematic_shiny()
        net2 <- delete.vertices(net,V(net)$parent!=input$var)
        
        
        ggraph(net2, layout="fr") + theme_solid() +theme(legend.position="none") +
            geom_edge_link(aes(color=TYPE)) + 
            geom_node_point(aes(color=type, size=5)) +
            geom_node_text(aes(label=label), repel=TRUE) +
            guides(shape=FALSE,color=FALSE,size=FALSE)
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
