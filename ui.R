if (!require(Rcpp))
  install.packages("Rcpp")
if (!require(shiny))
    install.packages('shiny')
# make sure you load DT *after* shiny
if (!require(DT))
    install.packages('DT')
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
#BiocManager::install()
#source("https://bioconductor.org/biocLite.R")
if (!require(uuid))
    install.packages('uuid')
if (!require(shinyBS))
    install.packages('shinyBS')
if (!require(networkD3))
    install.packages('networkD3')
if (!require(shinyjs))
    install.packages('shinyjs')
if (!require(magrittr))
    install.packages('magrittr')
if (!require(devtools))
  install.packages('devtools')
if (!require(colourpicker))
  install.packages('colourpicker')
if (!require(backports))
  install.packages('backports')

if (!require(viridis))
  install.packages('viridis')
if (!require(tidyverse))
  install.packages('tidyverse')
if (!require(plyr))
  install.packages('plyr')
if (!require(dplyr))
  install.packages('dplyr')
if (!require(purrr))
  install.packages('purrr')
if (!require(igraph))
  install.packages('igraph')
if (!require(RColorBrewer))
  install.packages('RColorBrewer')
if (!require(data.table))
  install.packages('data.table')
if (!require(stringi))
  install.packages('stringi')
if (!require(shinyjs))
  install.packages('shinyjs')

if (!require(rbokeh))
  install.packages('rbokeh')
if (!require(zoom))
  install.packages('zoom')
if (!require(shinythemes))
  install.packages('shinythemes')
if (!require(rhandsontable))
  install.packages('rhandsontable')


library(shinythemes)

library(BiocManager)
library(devtools)
library(shiny)
library(shinyBS)
library(DT)
library(uuid)
library(networkD3)
library(magrittr)
library(shinyjs)
library(colourpicker)
library(shinyWidgets)

library(viridis)
library(tidyverse)
library(plyr)
library(tidyr)
library(dplyr)
library(purrr)
library(igraph)
library(ggplot2)
library(ggraph)
library(RColorBrewer)
library(data.table)
library(networkD3)
library(stringi)

library(reshape2)
library(shinyjs)

library(visNetwork)
library(shinyWidgets)

library(r2d3)
library(jsonlite)



source("layouts_ui.R")
source("automated_annotations_vector.R")
source("statistics.R")

ui_options <- c("ui_table_line_height" = "80%")

ui_css <- paste0(
    '
    .box-panel {
    border-radius: 0 0 5px 5px;
    border-width: 1px;
    border-color: #d7d7d7;
    border-style: none solid solid solid;
    }
    .box-panel-padding {
    padding: 20px 20px 20px 20px;
    }
    .tabBox-panel {
    padding: 20px 20px 20px 20px;
    border-width: 1px;
    border-color: #d7d7d7;
    border-radius: 0px 0px 8px 8px;
    border-style: none solid solid solid;
    background: #ffffff;
    }

    .centerBlock {
    float: none;
    margin: 0 auto;
    }

    ',
    ".dataTables_wrapper td {
    line-height: ",
    ui_options['ui_table_line_height'],
    ";
    }"
)



ui_dataTable_panel <- function(datasetName, pagination = TRUE) {
    return(
        parse(
            text = paste0("div(div(DT::dataTableOutput('", datasetName, "'), class='box-panel-padding'), class='box-panel')"
            )
        )
    )
}

fixedPage(
  theme = shinytheme("sandstone"),
  # shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
  tags$head(
    tags$style(HTML(css_generated)),
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    
  ),
    useShinyjs(),
    tags$head(tags$script(src = "cyjs.js")),
    navbarPage(
      "NORMA: The NetwORk Makeup Artist",
        header = tags$head(tags$style(type = "text/css", ui_css)),
        tabPanel(
            "Welcome",
            h2("Welcome to the NetwORk Makeup Artist"),
            strong(
                "a tool for visualization of annotation groups."
            ),
            br(),
            br(),
            helpText(
              "Create at least 2 files:
            a) one will be your network file in tab delimited format: 
            2 columns named 'from' and 'to',
            b) the other(s) will be your annotation file(s) in tab delimited format: 
            2 columns - the first column will be the annotations and the second will be network's nodes seperated with comma.
            Now, follow the steps below:"
            ),
            tags$ul(
                tags$li("Load your network file"),
                tags$li("Load your annotation(s) file"),
                tags$li("Give it a name"),
                tags$li("Hit the ADD button")
            )
        ),# tabPanel 'Welcome'
        
      tabPanel(
            "Upload",
            icon = icon("upload"),
            sidebarLayout(
                sidebarPanel(
                    bsAlert("tabUploadSideAlert"),
                    helpText("Please follow the steps below:"),
                    tags$ul(
                        tags$li("Load your network file in tab delimited format"),
                        tags$li("Choose the network type"),
                        tags$li("Give it a name"),
                        tags$li("Hit the ADD button")
                    ),
                    selectInput(
                      "uiLoadGraphOptionsInput",
                      "1: Choose Network",
                      c(
                        "File upload" = "oF",
                        "STRING" = "oR_String_interactions",
                        "Drosophila" = "oR_Drosophila"
                      )),
                    uiOutput("uiLoadGraphOptionsOutput"),
                    div(
                        span(
                          actionButton("btnAddNetwork", "ADD", icon = icon("plus")), class = "input-group-btn"
                        ),
                        tags$input(
                          id = "networkName",
                          type = "text",
                          class = "form-control",
                          placeholder = "Type network name ..",
                          value = "Network name"
                          ),
                        class = "input-group"
                    ),
                    uiOutput("uiStoredGraphsOutputRadio"),
                    hr(),
                    
                    ######2nd button-annotation
                    selectInput(
                      "uiLoadGraphOptionsInput2",
                      "2: Choose Annotation(s)",
                      c(
                        "File upload" = "oF",
                        "STRING_Annotation" = "oR_String_Annotation",
                        "Drosophila_Annotation" = "oR_Drosophila_Annotation"
                      )),
                    uiOutput("uiLoadGraphOptionsOutput2"),
                    div(
                      span(
                        actionButton("btnAddNetwork2", "ADD", icon = icon("plus")), class = "input-group-btn"
                      ),
                      tags$input(
                        id = "annotationName",
                        type = "text",
                        class = "form-control",
                        placeholder = "Type annotation name ..",
                        value = "Annotation name"
                      ),
                      class = "input-group"
                    ),
                    uiOutput("uiStoredGraphsOutputRadio2"),
                    hr(),
                    
                    
                    ######3rd button-expression
                    selectInput(
                      "uiLoadExpressions",
                      "3: Choose Expression - file",
                      c(
                        "File upload" = "oF",
                        "Expression_file" = "oR_Expression_file"
                      )),
                    uiOutput("uiLoadExpressions"),
                    div(
                      span(
                        actionButton("btnAddExpression", "ADD", icon = icon("plus")), class = "input-group-btn"
                      ),
                      tags$input(
                        id = "expressionName",
                        type = "text",
                        class = "form-control",
                        placeholder = "Type expression name ..",
                        value = "Expression name"
                      ),
                      class = "input-group"
                    ),
                    uiOutput("uiStoredGraphsOutputRadioEx"),
                    hr()
                ), #sidebarPanel
                
                ##############################################################
                
                mainPanel(
                  conditionalPanel(
                        condition = "input.availableNetworks == 0",
                        bsAlert("tabUploadMainAlert"),
                        uiOutput("uiStoredGraphsOutputSelectUpload")),
                        
                    conditionalPanel(
                      condition = "input.availableAnnotations == 0",
                      bsAlert("tabUploadMainAlert2"),
                      uiOutput("uiStoredGraphsOutputSelectUpload2")),
                  
                  
                  
                    tabsetPanel(
                      tabPanel(
                        "Table View of Network(s)",
                        icon = icon("table"),
                        eval(ui_dataTable_panel('datasettab1'))
                        ),
                      tabPanel(
                        "Table View of Annotation(s)",
                        icon = icon("table"),
                        eval(ui_dataTable_panel('datasettab2'))
                        )
                      ) #tabsetPanel
                ) # mainPanel
            ) # sidebarLayout
        ), # tabPanel 'Upload File(s)'
      
      tabPanel("Network",
               conditionalPanel(
                 condition = "input.availableNetworks == 0",
                 bsAlert("tabUploadMainAlert"),               
                 uiOutput("uiLoadGraphOptionsOutput_just_network")),
               br(),
               br(),
               tabsetPanel(
               tabPanel("Interactive Network",
                        
                          visNetworkOutput('tabVizIgraphSimple'),
                          class = 'box-panel-padding',
                        
                        class = 'box-panel',
               ),
               
               tabPanel("Modulariy/Clustering",
                        br(),
                        selectInput("automated_annotations",
                                    "The automated_annotations:",
                                    choices = automated_annotations_ui,
                                    selected = selected_automated_annotations,
                                    multiple = FALSE
                        ),
                        plotOutput("modularity_plot"),
                        eval(ui_dataTable_panel('Modularity_table'))

               )
               )#tabsetpanel
               ),
      tabPanel("Annotations",
               conditionalPanel(
                 condition = "input.availableNetworks == 0",
                 bsAlert("tabUploadMainAlert"),               
                 uiOutput("uiLoadGraphOptionsOutput_annotations_tab")),
               conditionalPanel(
                 condition = "input.availableAnnotations == 0",
                 bsAlert("tabUploadMainAlert"),               
                 uiOutput("uiLoadGraphOptionsOutput2_annotations_tab")),
                 conditionalPanel(
                   condition = "input.availableExpressions == 0",
                   bsAlert("tabUploadMainAlert3"),
                   uiOutput("uiStoredGraphsOutputSelectUploadExpressions")),
               br(),
               tabsetPanel(
                 
               tabPanel("Convex Hull",
                        br(),
               # Layouts #
               helpText("Select the layout for the analysis:"),
               
               selectInput("layouts",
                           "The layouts:",
                           choices = layouts_ui,
                           selected = selected_layouts,
                           multiple = FALSE
               ),
               hr(),
               prettyCheckbox(inputId = "show_labels",
                              label = "Show Labels",
                              thick = TRUE,
                              shape = "curve",
                              animation = "pulse",
                              status = "info",
                              inline = F,
                              value = T),
               prettyCheckbox(inputId = "expressions",
                              label = "Show expression colors",
                              thick = TRUE,
                              shape = "curve",
                              animation = "pulse",
                              status = "info",
                              inline = F,
                              value = F),
               prettyCheckbox(inputId = "some_labels",
                              label = "Show only chosen labels",
                              thick = TRUE,
                              shape = "curve",
                              animation = "pulse",
                              status = "info",
                              inline = F,
                              value = F),
               hr(),
               uiOutput("interactive_convex_hulls"),
               eval(ui_dataTable_panel("chooseGroups")),
               
               div(style="display:inline-block",sliderInput(inputId="scaling_coordinates_convex", label="Scale the coordinates:",min = 1, max = 10,
                                                            value = 1)),
               div(style="display:inline-block",sliderInput("scaling_nodes_convex", "Adjust node size:",
                                                            min = 0.2, max = 5,
                                                            value = 2, step= 0.2)),
               div(style="display:inline-block",sliderInput("scaling_labels_convex", "Adjust label size:",
                                                            min = 0, max = 30,
                                                            value = 10, step= 2)),
               

               ),
           
               tabPanel("Pie - Chart Nodes",
                        fluidRow(
                          helpText("Select the layout you want in the analysis."),
                          selectInput("layouts2",
                                      "The layouts:",
                                      choices = layouts_ui,
                                      selected = selected_layouts,
                                      multiple = FALSE
                          ),
                          hr(),
                          ###############
                          prettyCheckbox(inputId = "show_labels_pies",
                                         label = "Show Labels",
                                         thick = TRUE,
                                         shape = "curve",
                                         animation = "pulse",
                                         status = "info",
                                         inline = F,
                                         value = T),
                          prettyCheckbox(inputId = "expressions_pies",
                                         label = "Show expression colors",
                                         thick = TRUE,
                                         shape = "curve",
                                         animation = "pulse",
                                         status = "info",
                                         inline = F,
                                         value = F),
                          prettyCheckbox(inputId = "some_labels_pies",
                                         label = "Show only chosen labels",
                                         thick = TRUE,
                                         shape = "curve",
                                         animation = "pulse",
                                         status = "info",
                                         inline = F,
                                         value = F),
                          # plotOutput('tabVizPie_charts'),
                          uiOutput('tabVizPie_charts'),
                          eval(ui_dataTable_panel("chooseGroups2")),
                        ),
                        
                        div(style="display:inline-block",sliderInput("scaling_coordinates_pies", "Scale the coordinates",
                                                                     min = 1, max = 10,
                                                                     value = 1)),
                        div(style="display:inline-block",sliderInput("scaling_nodes_pies", "Adjust node size:",
                                                                     min = 10, max = 30,
                                                                     value = 15)),
                        div(style="display:inline-block",sliderInput("scaling_labels_pies", "Adjust label size:",
                                                                     min = 0, max = 30,
                                                                     value = 10, step= 2)),
                        
                        
                        hr(),
                        # downloadButton("pie_chartsDownload"),
                        class = 'box-panel-padding',
                        class = 'box-panel'
                        
               )
               
               ) #tabsetPanel
               
               ),
      
        tabPanel(
            "Topology",
            icon = icon("globe", lib = "glyphicon"),
            sidebarLayout(
                sidebarPanel(
                    bsAlert("tabTopologySideAlert"),
                    conditionalPanel(
                        condition = "input.statisticsMethodsMainTabsetPanel == 'tableView'",
                        uiOutput("uiStoredGraphsOutputSelectTopolopgy")
                    ),
                    conditionalPanel(
                        condition = "input.statisticsMethodsMainTabsetPanel == 'plotView'",
                        uiOutput("uiStoredGraphsOutputMultipleSelectTopolopgy")
                    ),
                ),
                conditionalPanel(
                    condition = "input.availableNetworks == 0",
                    mainPanel(
                        bsAlert("tabTopologyMainAlert"),
                        tabsetPanel(
                            id = "statisticsMethodsMainTabsetPanel",
                            tabPanel(
                              "Summaries",
                              value = "tableView",
                              icon = icon("table"),
                                div(
                                    div(checkboxGroupInput(
                                      "statistics2",
                                      "The statistics:",
                                      choices = statistics,
                                      selected = selected_statistics
                                    ),
                                    eval(ui_dataTable_panel("statres", FALSE)),
                                         class = 'box-panel-padding',
                                    class = 'box-panel'
                                ),
                                div(
                                  span(actionButton("btnStatSelectAll", "Select all", style = "float: right;"), class = "input-group-btn"),
                                  span(actionButton("btnStatSelectNone", "Clear"), class = "input-group-btn"),
                                  class = "input-group"
                                )
                            )),
                            tabPanel(
                                "Comparative Plots",
                                value = "plotView",
                                icon = icon("bar-chart"),
                                div(
                                    div(radioButtons(
                                      "statistics",
                                      "The statistics:",
                                      choices = statistics,
                                      selected = selected_statistics
                                    ),
                                        uiOutput("statisticsMethodsPlotRender"),
                                        class = 'box-panel-padding'
                                    ), class = 'box-panel'
                                ),
                                sliderInput(
                                    "statisticsPlotPercentMagnify",
                                    "Adjust plot height (% taller)",
                                    min = 0,
                                    max = 100,
                                    value = 0
                                ),
                                span(
                                    actionButton("btnRefreshPalette", "Refresh palette", icon = icon("refresh"))
                                )
                            )
                            
                        )
                    )
                )
            )
        ),
       

        
        tabPanel(
            "Help",
            icon = icon("question"),
            strong("Number of Edges"),
            helpText(
            "Shows the number of edges in the network. If the has more than 10000 edges it will take into account the first 10000."
            ),
            br(),
            strong("Number of Nodes"),
            helpText(
            "Shows the number of noded in the network. There is no limitation on the nuber of nodes."
            ),
            br(),
            strong("Diameter"),
            helpText(
            "Shows the length of the longest geodesic.The diameter is calculated by using a breadth-first search like method.The graph-theoretic or geodesic distance between two points is defined as the length of the shortest path between them."
            ),
            br(),
            strong("Radius"),
            helpText(
            "The eccentricity of a vertex is its shortest path distance from the farthest other node in the graph. The smallest eccentricity in a graph is called its radius. The eccentricity of a vertex is calculated by measuring the shortest distance from (or to) the vertex, to (or from) all vertices in the graph, and taking the maximum."
            ),
            br(),
            strong("Density"),
            helpText(
            "The density of a graph is the ratio of the number of edges and the number of possible edges."
            ),
            br(),
            strong("Number of Edges"),
            helpText(
            "Shows the number of edges in the network. If the has more than 10000 edges it will take into account the first 10000."
            ),
            br(),
            strong("Average path length"),
            helpText("The average number of steps needed to go from a node to any other."),
            br(),
            strong("Clustering Coefficient:"),
            helpText("A metric to show if the network has the tendency to form clusters."),
            br(),
            strong("Modularity"),
            helpText(
            "This function calculates how modular is a given division of a graph into subgraphs."
            ),
            br(),
            strong("Number of self loops"),
            helpText("how many nodes are connected to themselves."),
            br(),
            strong("Average Eccentricity:"),
            helpText(
            "The eccentricity of a vertex is its shortest path distance from the farthest other node in the graph."
            ),
            br(),
            strong("Average Eigenvector Centrality:"),
            helpText("It is a measure of the influence of a node in a network."),
            br(),
            strong("Assortativity degree:"),
            helpText(
            "The assortativity coefficient is positive is similar vertices (based on some external property) tend to connect to each, and negative otherwise."
            ),
            br(),
            strong("Is directed acyclic graph"),
            helpText("It returns True (1) or False (0)."),
            br(),
            strong("Is directed"),
            helpText(
            "It returns True (1) or False (0) depending whether the edges are directed or not."
            ),
            br(),
            strong("Is Bipartite"),
            helpText(
            "It returns True (1) or False (0) depending whether the graph is bipartite or not."
            ),
            br(),
            strong("Is chordal"),
            helpText(
            "It returns True (1) or False (0). A graph is chordal (or triangulated) if each of its cycles of four or more nodes has a chord, which is an edge joining two nodes that are not adjacent in the cycle. An equivalent definition is that any chordless cycles have at most three nodes."
            ),
            br(),
            strong("Average number of Neighbors"),
            helpText("How many neighbors each node of the network has on average."),
            br(),
            strong("Centralization betweenness"),
            helpText(
            "It is an indicator of a node's centrality in a network. It is equal to the number of shortest paths from all vertices to all others that pass through that node.Betweenness centrality quantifies the number of times a node acts as a bridge along the shortest path between two other nodes."
            ),
            br(),
            strong("Centralization closeness"),
            helpText(
            "It measures the speed with which randomly walking messages reach a vertex from elsewhere in the graph."
            ),
            br(),
            strong("Centralization degree"),
            helpText("It is defined as the number of links incident upon a node."),
            br(),
            strong("Graph mincut"),
            helpText(
            "Calculates the minimum st-cut between two vertices in a graph. The minimum st-cut between source and target is the minimum total weight of edges needed to remove to eliminate all paths from source to target."
            ),
            br(),
            strong("Motifs-3"),
            helpText(
              "Searches a graph for motifs of size 3."
            ),
            br(),
            strong("Motifs-4"),
            helpText(
              "Searches a graph for motifs of size 4."
            ),
            br(),
            br()
        ),
        # tabPanel 'Help'
        tabPanel(
            "Input File",
            strong("The input file can be a network in tab delimited format."),
            br(),
            helpText(
                "Below you can see a network, an annotation and node-coloring example in tab delimited format. In case that there is not a node-coloring file, you can completely skip it"
            ),
            pre(
                "
Network File:             Annotation File:                        Node-coloring file            Warnings!
                                  
from    to                Group-2 BCL2L1,MDM4,MDM2,CHEK2          CDKN1A  blue                - Network file: Must have headers: from - to
CDKN1A  TP53              Group-5 TP53,EP300                      TP53    blue                - Annotation file: Without headers and without
TP53	MDM2              Group-1 CDKN2A,ATM,TP53BP2,MDM2         MDM4    blue                  spaces between Nodes - only commas
MDM4	TP53              Group-4 CHEK2,CREBBP,MDM2               BCL2L1  red                   (e.g. BCL2L1,MDM4,MDM2)
BCL2L1	TP53              Group-3 TP53,BCL2L1                     CHEK2   red                 - Node-coloring file: Without headers and the
CHEK2   ATM               Group-6 MDM4,MDM2                       ATM     red                   colors should be some of the basic colors
TP53    EP300                                                     TP53BP2 red                   (e.g. red, green, yellow, blue, navy, 
ATM	TP53                                                      CDKN2A  blue                  orange, purple, black, aqua, etc.)
TP53    CREBBP                                                    EP300   red                   
MDM4    MDM2                                                      CREBBP  red
CHEK2	TP53                                                      MDM2    blue
TP53BP2	TP53                                          
CDKN2A	TP53
CDKN2A	MDM2
ATM	MDM2
EP300	CREBBP
  .       .
  .       .
  .       .
"
            )
        ),
        tabPanel(
            "People",
            icon = icon("users"),
            strong("The Team:"),
            tags$ul(
                tags$li("Mikaela Koutrouli - BSRC 'Alexander Fleming'"),
                tags$li("Georgios Pavlopoulos - BSRC 'Alexander Fleming'")
            ),
            br()
        )
    )
)
