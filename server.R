# By default, Shiny limits file uploads to 5MB per file. You can modify
# this limit by using the shiny.maxRequestSize option. For example,# adding options(shiny.maxRequestSize=30*1024^2) to the top of server.R
# would increase the limit to 30MB. From within R, inside the app
# source dir, start the app with: shiny::runApp(getwd()) From a shell,# start the app with: R -e 'shiny::runApp('~/appsourcedir')'

if (!require(shiny)) install.packages("shiny")
# make sure you load DT *after* shiny
if (!require(DT)) install.packages("DT")
if (!require(igraph)) install.packages("igraph")
if (!require(plyr)) install.packages("plyr")
if (!require(d3Network)) install.packages("d3Network")
if (!require(uuid)) install.packages("uuid")
if (!require(lattice)) install.packages("lattice")
if (!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics")
if (!require(shinyBS)) install.packages("shinyBS")
if (!require(networkD3)) install.packages("networkD3")
if (!require(shinyjs)) install.packages("shinyjs")
if (!require(devtools)) install.packages("devtools")
if (!require(randomcoloR)) install.packages("randomcoloR")
if (!require(Cairo)) install.packages("Cairo")


if (!require(GraphAlignment)){
  source("https://bioconductor.org/biocLite.R")
  biocLite("GraphAlignment")
}

library(devtools)
library(shiny)
library(shinyBS)
library(DT)
library(igraph)
library(plyr)
library(d3Network)
library(uuid)
library(lattice)
library(networkD3)
library(shinyjs)
library(Cairo)
library(randomcoloR)
library(MCL)
library(colourpicker)
library(GraphAlignment)

library(reshape2)
library(shinyjs)

library(reactlog) #debugging

options(shiny.usecairo = F)
options(shiny.maxRequestSize=30*1024^2) #30 MB for uploaded networks
options(shiny.reactlog=TRUE) #debugging

source("statistics.R")
source("netstats.R")
source("layout_choices.R")
source("layouts_ui.R")

max_edges = 50000
colors <- randomColor(50)




read_data <- function(datapath, type = c("txt"), header = T, sep = "\t", quote = "\"", weighted = F, directed = F) ({
    dataset1 <- read.table(datapath, header = header, sep = sep, quote = quote)
    # if(weighted)
    if (ncol(dataset1) == 2) {
        dataset1$V3 <- 1
    } else if (ncol(dataset1) > 3) {
        dataset1 <- dataset1[, 1:3]
    } else if (ncol(dataset1) != 3)
        return(NULL)
    
    colnames(dataset1) <- c("Source", "Target", "Weight")
    #else colnames(dataset1) <- c('Source', 'Target')
    return(dataset1)
})


read_annotations <- function(datapath, type = c("txt"), header = F, sep = "\t", quote = "\"", weighted = F, na.strings=c("","NA")) ({
  annotation1 <- read.table(datapath, header = header, sep = sep, quote = quote)

})

read_expressions <- function(datapath, type = c("txt"), header = F, sep = "\t", quote = "\"", weighted = F, na.strings=c("","NA")) ({
  expression1 <- read.table(datapath, header = header, sep = sep, quote = quote)
  
})


convert_to_igraph <- function(dataset1) ({
    # directed_tf <- FALSE
    # if (attr(dataset1, "directed")) {
    #     directed_tf <- attr(dataset1, "directed")
    # }
    igraph <- graph.data.frame(dataset1, vertices = NULL)
    
    return(igraph)
})

EmptyDataset <- function(columns) {
    dataset <- data.frame(V1 = integer())
    lapply(columns[-1], function(x) dataset[, x] <<- integer())
    colnames(dataset) <- columns
    return(dataset)
}

ui_options <- c(ui_table_font_sz = "80%") #html size



options(shiny.error = browser) #debugging





shinyServer(function(input, output, session) {
    reactiveVars <- reactiveValues()
    reactiveVars$StoredNetworks <- data.frame(id = character(), name = character(), stringsAsFactors = F)
    reactiveVars$SelectedStoredNetworksIds <- c()
    #
    reactiveVars$StoredAnnotations <- data.frame(id = character(), name = character(), stringsAsFactors = F)
    reactiveVars$SelectedStoredAnnotationIds <- c()
    reactiveVars$StoredExpressions <- data.frame(id = character(), name = character(), stringsAsFactors = F)
    reactiveVars$SelectedStoredExpressionIds <- c()
    #
    reactiveVars$layoutCoord <- NULL
    reactiveVars$plotData <- NULL
    reactiveVars$last_intersection_data <- NULL

    session$onSessionEnded(function() {
        snets <- isolate(StoredNets())
        if (nrow(snets) > 0) {
            unlink(c("*.rda", "*.zip"))
        }
    })
    
    session$onSessionEnded(function() {
      sannots <- isolate(StoredAnnots())
      if (nrow(sannots) > 0) {
        unlink(c("*.rda", "*.zip"))
      }
    })
    
    session$onSessionEnded(function() {
      sexpress <- isolate(StoredExpress())
      if (nrow(sexpress) > 0) {
        unlink(c("*.rda", "*.zip"))
      }
    })
    
############ Read the files ################
loadNetworkFromFile <- function() {
    dataset1 <- NULL
    switch(input$uiLoadGraphOptionsInput, oF = {
        if (!is.null(input$file1)) {
            dataset1 <- read_data(input$file1$datapath)
        }
    }, oR_String_interactions = {
        dataset1 <- read.delim("C:/Users/mikae/Desktop/MSc_Molecular_Biomedicine/Master_Thesis_Pavlopoulos/NAP/NORMA_project/string_interactions.txt", header= T)
    }, oR_Drosophila = {
        n <- as.integer(input$oR_selected_size)
        dataset1 <- read.delim("C:/Users/mikae/Desktop/MSc_Molecular_Biomedicine/Master_Thesis_Pavlopoulos/NAP/NORMA_project/PAP_example.txt")
    }
    )
        if (input$uiLoadGraphOptionsInput != "oF" && !is.null(dataset1)) {
            # if(input$weighted1) {
            if (!is.null(dataset1))
                dataset1$X3 <- sample(1:10, nrow(dataset1), replace = T)
            colnames(dataset1) <- c("Source", "Target", "Weight")
            # } else { colnames(dataset1) <- c('Source', 'Target') }
        }
        return(dataset1)
}
    
    output$uiLoadGraphOptionsOutput <- renderUI({
        if (is.null(input$uiLoadGraphOptionsInput))
            return()

        # Depending on input$input_type, we'll generate a different UI
        # component and send it to the client.
        if (input$uiLoadGraphOptionsInput == "oF") {
            wellPanel(fileInput("file1", "Choose file to upload", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")))
        }
      #, checkboxInput(inputId = "directed1", "Directed", value = FALSE)
        })
    

    #2nd-annotations
    
    loadAnnotations <- function() {
      annotation1 <- NULL
      switch(input$uiLoadGraphOptionsInput2, oF = {
        if (!is.null(input$file2)) {
          annotation1 <- read_annotations(input$file2$datapath)
        }
      }, oR_String_Annotation = {
          annotation1 <- read.delim("C:/Users/mikae/Desktop/MSc_Molecular_Biomedicine/Master_Thesis_Pavlopoulos/NAP/NORMA_project/string_interactions_groups_comma_duplicate.txt", header= F)
      }, oR_Drosophila_Annotation = {
        n <- as.integer(input$oR_selected_size)
                annotation1 <- read.delim("C:/Users/mikae/Desktop/MSc_Molecular_Biomedicine/Master_Thesis_Pavlopoulos/NAP/NORMA_project/PAP_david.txt", header = F)
      }
      )
      
      # if (input$uiLoadGraphOptionsInput2 != "oF" && !is.null(annotation1)) {
      #   if (!is.null(annotation1))
      #     annotation1$X3 <- sample(1:10, nrow(annotation1), replace = T)
      # }
      colnames(annotation1) <- c("Annotations", "Nodes")
      return(annotation1)
    }
    
    
    output$uiLoadGraphOptionsOutput2 <- renderUI({
      if (is.null(input$uiLoadGraphOptionsInput2))
        return()
      
      # Depending on input$input_type, we'll generate a different UI
      # component and send it to the client.
      if (input$uiLoadGraphOptionsInput2 == "oF") {
        wellPanel(fileInput("file2", "Choose file to upload", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")))
      }
      
    })
    
    
    
    
    ####################################

    #Change of Network Name based on input choices
    dochangeNetworkName<- observe({
      updateTextInput(session, 
                      inputId = "networkName", 
                      value = (
                        if (input$uiLoadGraphOptionsInput == "OF"){
                          paste("Network name")
                        })
                      )
      })
    
    dochangeNetworkName2<- observe({
      updateTextInput(session, 
                      inputId = "networkName", 
                      value = (
                        if (input$uiLoadGraphOptionsInput == "oR_String_interactions"){
                          paste("STRING")
                        })
                      )
    })
    
    dochangeNetworkName3<- observe({
      updateTextInput(session, 
                      inputId = "networkName", 
                      value = (
                        if (input$uiLoadGraphOptionsInput == "oR_Drosophila"){
                          paste("Drosophila")
                        })
                      )
    })
    
    #####################################################
    #Change of Annotation Name based on input choices
    dochangeAnnotationName<- observe({
      updateTextInput(session, 
                      inputId = "annotationName", 
                      value = (
                        if (input$uiLoadGraphOptionsInput2 == "OF"){
                          paste("Annotation name")
                        })
      )
    })
    
    dochangeAnnotationName2<- observe({
      updateTextInput(session, 
                      inputId = "annotationName", 
                      value = (
                        if (input$uiLoadGraphOptionsInput2 == "oR_String_Annotation"){
                          paste("STRING Annotation")
                        })
      )
    })
    
    dochangeAnnotationName3<- observe({
      updateTextInput(session, 
                      inputId = "annotationName", 
                      value = (
                        if (input$uiLoadGraphOptionsInput2 == "oR_Drosophila_Annotation"){
                          paste("Drosophila Annotation")
                        })
      )
    })
    
    
   ###################################################### 

    StoredNets <- reactive({
        return(reactiveVars$StoredNetworks)
    })
    
    SelectedStoredNets <- function() {
        if (length(reactiveVars$SelectedStoredNetworksIds) > 0) 
            return(StoredNets()[which(reactiveVars$StoredNetworks$id %in% 
                reactiveVars$SelectedStoredNetworksIds), ]) else if (nrow(StoredNets()) == 0 || is.na(StoredNets()[1, ])) 
            return(NULL) else {
            updateCheckboxGroupInput(session, "storedGraphsOutputMultipleSelectTopolopgy", "Selected network(s)", choices = getStoredNetsChoices(), selected = getStoredNetsChoices()[1])
            return(StoredNets()[1, ])
        }
    }
    
    ###########################################
    
    
    
    StoredNetsEmpty <- function() {
        return(nrow(reactiveVars$StoredNetworks) == 0)
    }
    
    getDatasetName <- function(id) {
        sn <- StoredNets()
        idi <- which(sn$id == id)
        if (is.null(sn) || nrow(sn) == 0 || length(idi) == 0) 
            return(NULL)
        return(sn[idi, ]$name)
    }
    
    fetchDataset <- function(nid) {
        retVal <- NULL
        if (length(nid) > 0) {
            retVal <- readRDS(paste0(nid, ".rda"))
            attr(retVal, "id") <- nid
        }
        return(retVal)
    }
    
    fetchFirstSelectedStoredDataset <- reactive({
        ssn <- SelectedStoredNets()
        if (!is.null(ssn) && nrow(ssn) > 0) {
            return(fetchDataset(ssn[1, ]$id))
        } else {
            return(NULL)
        }
    })
    
    fetchFirstSelectedStoredIgraph <- function() {
        dataset <- fetchFirstSelectedStoredDataset()
        if (is.null(dataset)) 
            return(NULL) else return(convert_to_igraph(dataset))
    }
    
    fetchAllSelectedStoredDataset <- function() {
        ssn <- SelectedStoredNets()
        ids <- c()
        if (!is.null(ssn) && nrow(ssn) > 0) {
            ret <- list()
            for (i in 1:nrow(ssn)) {
                ret[[i]] <- fetchDataset(ssn[i, ]$id)
                ids <- c(ids, ssn[i, ]$id)
            }
            names(ret) <- ids
            return(ret)
        } else {
            return(NULL)
        }
    }
    
    fetchMaxNSelectedStoredDataset <- function(N) {
        ssn <- SelectedStoredNets()
        if (!is.null(ssn) && nrow(ssn) > 0) {
            ret <- list()
            for (i in 1:nrow(ssn)) {
                if (i > N) 
                  break
                ret[[i]] <- fetchDataset(ssn[i, ]$id)
            }
            return(ret)
        } else {
            return(NULL)
        }
    }
    
    fetchMaxTwoSelectedStoredIgraphs <- function() {
        datasets <- fetchMaxNSelectedStoredDataset(2)
        if (is.null(datasets) || length(datasets) == 0) 
            return(NULL) else {
            ret <- list()
            for (i in length(datasets)) ret[[i]] <- convert_to_igraph(datasets[[i]])
            return(ret)
        }
    }
    
    
    
    fetchFirstSelectedStoredAnnotations <- function() {
      groups_ann <- fetchFirstSelectedStoredGroups()
      if (is.null(groups_ann)) 
        return(NULL) else return(groups_ann)
    }
    
    fetchFirstSelectedStoredGroups <- reactive({
      sannots <- SelectedStoredAnnots()
      if (!is.null(sannots) && nrow(sannots) > 0) {
        return(fetchDataset(sannots[1, ]$id))
      } else {
        return(NULL)
      }
    })
    
    ###### Upload #######
    doAddNetwork <- observeEvent(input$btnAddNetwork, {
      dataset <- loadNetworkFromFile()
        if (!is.null(dataset)) {
            nid <- UUIDgenerate(T)      #time-base UUID is generated
            nn <- input$networkName 
            cnt <- 1                    #count
            while (nn %in% reactiveVars$StoredNetworks$name) {  #reactiveVars: represents a single reactive variable. 
                cnt <- cnt + 1
                nn <- paste(input$networkName, cnt)      #paste: converts its arguments (via as.character) to character strings
            }
            df <- data.frame(id = nid, name = nn, stringsAsFactors = F)
            if (nrow(dataset) > 10000) {
                dataset <- dataset[1:10000, ]
            }
            # attr(dataset, which = "directed") <- input$directed1
            # attr(dataset, which = 'weighted') <- input$weighted1
            
            reactiveVars$StoredNetworks <- rbind(reactiveVars$StoredNetworks, df)
            saveRDS(dataset, paste0(nid, ".rda"))
            if (length(reactiveVars$SelectedStoredNetworksIds) == 0) {
                reactiveVars$SelectedStoredNetworksIds <- c(nid)
            }
        } else createAlert(session, "tabUploadSideAlert", "fileUploadAlert", title = "ERROR !", style = "danger", content = paste0("An error occurred while trying to read your file. Please make sure that it is formatted according to the requirements."), append = FALSE)
      })
    
    doRemNetwork <- observeEvent(input$btnRemoveNetworks, {
        if (!is.null(input$availableNetworks)) {
            reactiveVars$StoredNetworks <- reactiveVars$StoredNetworks[-which(reactiveVars$StoredNetworks$id %in% 
                input$availableNetworks), ]
            nr <- nrow(reactiveVars$StoredNetworks)
            if (nr > 0) 
                reactiveVars$SelectedStoredNetworksIds <- reactiveVars$StoredNetworks[nr, ]$id
        }
    })
    
    getStoredNetsChoices <- function() {
        snets <- StoredNets()
        if (nrow(snets) == 0) 
            return(NULL)
        choices <- snets$id
        names(choices) <- snets$name
        return(choices)
    }
    
    # outputOptions(output, 'availableNetworks', suspendWhenHidden=FALSE)

    output$uiStoredGraphsOutputRadio <- renderUI({
        input$btnAddNetwork
        input$btnRemoveNetworks
        choices <- getStoredNetsChoices()
        if (is.null(choices)) 
            return()
        return(list(br(), wellPanel(h4("Available networks"), checkboxGroupInput("availableNetworks", label = "", choices = choices)), div(div(actionButton("btnRemoveNetworks", "REMOVE", icon = icon("minus")), class = "col-md-5 centerBlock"), class = "row text-center")))
        
    })
    
    uploadTabSetSelectedNetwork <- observeEvent(input$storedGraphsOutputSelectUpload, {
            reactiveVars$SelectedStoredNetworksIds <- c(input$storedGraphsOutputSelectUpload)
        }, ignoreNULL = FALSE)
    
    output$uiStoredGraphsOutputSelectUpload <- renderUI({
        input$btnAddNetwork
        input$btnRemoveNetworks
        choices <- getStoredNetsChoices()
        if (is.null(choices)) 
            return()
        return(selectInput("storedGraphsOutputSelectUpload", "Selected network", choices))
    })
    
    output$uiStoredGraphsOutputSelectUpload_just_network <- renderUI({
        input$btnAddNetwork
        input$btnRemoveNetworks
        choices <- getStoredNetsChoices()
        if (is.null(choices)) 
            return()
        return(selectInput("storedGraphsOutputSelectUpload", "Selected network", choices))
    })
    
    #########################################################
    ### Annotation button ####
    
    uploadTabSetSelectedNetwork2 <- observeEvent(input$storedGraphsOutputSelectUpload2, {
      reactiveVars$SelectedStoredAnnotationIds <- c(input$storedGraphsOutputSelectUpload2)
    }, ignoreNULL = FALSE)
    
    output$uiStoredGraphsOutputRadio2 <- renderUI({
      input$btnAddNetwork2
      input$btnRemoveNetworks2
      choices <- getStoredAnnotChoices()
      if (is.null(choices)) 
        return()
      return(list(br(), wellPanel(h4("Available Annotations"), checkboxGroupInput("availableAnnotations", label = "", choices = choices)), div(div(actionButton("btnRemoveNetworks2", "REMOVE", icon = icon("minus")), class = "col-md-5 centerBlock"), class = "row text-center")))
      
    })
    
    output$uiStoredGraphsOutputSelectUpload2 <- renderUI({
      input$btnAddNetwork2
      input$btnRemoveNetworks2
      choices <- getStoredAnnotChoices()
      if (is.null(choices)) 
        return()
      return(selectInput("storedGraphsOutputSelectUpload2", "Selected annotation", choices))
    })
    
    
    getStoredAnnotChoices <- function() {
      sannots <- StoredAnnots()
      if (nrow(sannots) == 0) 
        return(NULL)
      choices <- sannots$id
      names(choices) <- sannots$name
      return(choices)
    }
    
    StoredAnnots <- reactive({
      return(reactiveVars$StoredAnnotations)
    })
    
    
    doAddNetwork2 <- observeEvent(input$btnAddNetwork2, {
      annotation<- loadAnnotations()
      if (!is.null(annotation)) {
        nid <- UUIDgenerate(T)      #time-base UUID is generated
        nn <- input$annotationName 
        cnt <- 1                    #count
        while (nn %in% reactiveVars$StoredAnnotations$name) {  #reactiveVars: represents a single reactive variable. 
          cnt <- cnt + 1
          nn <- paste(input$annotationName, cnt)      #paste: converts its arguments (via as.character) to character strings
        }
        dtf <- data.frame(id = nid, name = nn, stringsAsFactors = F)
        if (nrow(annotation) > 10000) {
          annotation<- annotation[1:10000, ]
        }
        reactiveVars$StoredAnnotations <- rbind(reactiveVars$StoredAnnotations, dtf)
        saveRDS(annotation, paste0(nid, ".rda"))
        if (length(reactiveVars$SelectedStoredAnnotationIds) == 0) {
          reactiveVars$SelectedStoredAnnotationIds <- c(nid)
        }
      } else createAlert(session, "tabUploadSideAlert2", "annotationUploadAlert", title = "ERROR !", style = "danger", content = paste0("An error occurred while trying to read your file. Please make sure that it is formatted according to the requirements."), append = FALSE)
    })
       
    
    doRemNetwork2 <- observeEvent(input$btnRemoveNetworks2, {
      if (!is.null(input$availableAnnotations)) {
        reactiveVars$StoredAnnotations <- reactiveVars$StoredAnnotations[-which(reactiveVars$StoredAnnotations$id %in% 
                                                                            input$availableAnnotations), ]
        nr <- nrow(reactiveVars$StoredAnnotations)
        if (nr > 0) 
          reactiveVars$SelectedStoredAnnotationIds <- reactiveVars$StoredAnnotations[nr, ]$id
      }
    })
    
    ### Output after choosing selected annotations ###
    SelectedStoredAnnots <- function() {
      if (length(reactiveVars$SelectedStoredAnnotationIds) > 0) 
        return(StoredAnnots()[which(reactiveVars$StoredAnnotations$id %in% 
                                    reactiveVars$SelectedStoredAnnotationIds), ]) else if (nrow(StoredAnnots()) == 0 || is.na(StoredAnnots()[1, ])) 
                                      return(NULL) else {
                                        updateCheckboxGroupInput(session, "storedGraphsOutputMultipleSelectTopolopgy2", "Selected annotation(s)", choices = getStoredAnnotChoices(), selected = getStoredAnnotChoices()[1])
                                        return(StoredAnnots()[1, ])
                                      }
    }
    
    fetchFirstSelectedStoredDataset2 <- reactive({
      sannots <- SelectedStoredAnnots()
      if (!is.null(sannots) && nrow(sannots) > 0) {
        return(fetchDataset(sannots[1, ]$id))
      } else {
        return(NULL)
      }
    })
    
    output$datasettab2 <- DT::renderDataTable({
      annotation<- fetchFirstSelectedStoredDataset2()
      if (is.null(annotation)) 
        annotation<- EmptyDataset(c("Annotations", "Nodes"))
      datatable(annotation, rownames = FALSE, extensions = 'Responsive') %>% formatStyle(colnames(annotation), fontSize = ui_options["ui_table_font_sz"])
    })
    
    topologyTableViewSetSelectedNetwork <- observeEvent({
        input$storedGraphsOutputSelectTopolopgy
        input$statisticsMethodsMainTabsetPanel
    }, {
        if (input$statisticsMethodsMainTabsetPanel == "tableView" && !(length(reactiveVars$SelectedStoredNetworksIds) == 
            1 && reactiveVars$SelectedStoredNetworksIds == input$storedGraphsOutputSelectTopolopgy)) {
            reactiveVars$SelectedStoredNetworksIds <- c(input$storedGraphsOutputSelectTopolopgy)
        }
    }, ignoreNULL = FALSE)
    
    output$uiStoredGraphsOutputSelectTopolopgy <- renderUI({
        input$btnAddNetwork
        input$btnRemoveNetworks
        choices <- getStoredNetsChoices()
        if (is.null(choices)) 
            return()
        return(selectInput("storedGraphsOutputSelectTopolopgy", "Selected network", choices))
    })
    
    topologyPlotViewSetSelectedNetwork <- observeEvent({
        input$storedGraphsOutputMultipleSelectTopolopgy
        input$statisticsMethodsMainTabsetPanel
    }, {
        if (input$statisticsMethodsMainTabsetPanel == "plotView" && !(length(reactiveVars$SelectedStoredNetworksIds) == 
            length(input$storedGraphsOutputMultipleSelectTopolopgy) && 
            all(reactiveVars$SelectedStoredNetworksIds == input$storedGraphsOutputMultipleSelectTopolopgy))) {
            reactiveVars$SelectedStoredNetworksIds <- c(input$storedGraphsOutputMultipleSelectTopolopgy)
        }
    }, ignoreNULL = FALSE)
    
    output$uiStoredGraphsOutputMultipleSelectTopolopgy <- renderUI({
        input$btnAddNetwork
        input$btnRemoveNetworks
        choices <- getStoredNetsChoices()
        if (is.null(choices)) 
            return()
        return(checkboxGroupInput("storedGraphsOutputMultipleSelectTopolopgy", "Selected network(s)", choices))
    })
    
    
    
    
    ######## Plots and Download Buttons #######################
    output$tabVizIgraphSimple<- renderVisNetwork({
      # renderSimpleNetwork({
      g <- fetchFirstSelectedStoredIgraph()
      if (is.null(g)) 
        return()
      
      my_network<- as.data.frame(get.edgelist(g))
      my_network<- data.frame(from = my_network$V1, to = my_network$V2)
      
      visIgraph(as.undirected(g)) %>%
        visNodes(size = 25, shape = "ellipse") %>%
        visOptions(highlightNearest = TRUE, 
                   nodesIdSelection = TRUE) %>%
        visInteraction(keyboard = TRUE, navigationButtons = TRUE, zoomView = TRUE, multiselect =TRUE, dragView = TRUE)
      
      
      
      #
      # visGetBoundingBox(g_virt, id, input = paste0(g$id, "tabVizIgraphSimple"))
      
      #
      # if (length(my_network$from) < 200){
      #   simpleNetwork(my_network,
      #                 linkDistance = 200,
      #                 charge = -30,
      #                 fontSize = 12,
      #                 fontFamily = 'Arial',
      #                 linkColour = "#666", nodeColour = "#3182bd", opacity = 0.8, zoom = T)
      # } else 
      #   withProgress(min = 0, max = 1, {
      #     incProgress(message = "Processing data into plot", 
      #                 detail = "This may take a while...", amount = .1)
      #     simpleNetwork(my_network,
      #                   linkDistance = 50,
      #                   charge = -5,
      #                   fontSize = 9,
      #                   fontFamily = 'Arial',
      #                   linkColour = "#666", nodeColour = "#3182bd", opacity = 0.8, zoom = T)
      #   })
    })
    
    
    
    
    ### Pie - Charts ###
    
    output$chooseGroups2 <- DT::renderDataTable({
      annotation<- fetchFirstSelectedStoredDataset2()
      if (is.null(annotation)) 
        annotation<- EmptyDataset(c("Annotations", "Nodes"))
      rowCallback_generated <-"function(row, dat, displayNum, index){"
      for (i in 1: length(rownames(annotation)))
      {
        rowCallback_generated<-paste(rowCallback_generated, "if(dat[0]==",i,"){","$('td:eq(1)', row).addClass('x",i,"');}" ,sep="")
      }
      rowCallback_generated<-paste(rowCallback_generated, "}",sep="")
      
      css_colors <- group_pal_rows(length(rownames(annotation)))
      ########
      
      x<- length(rownames(annotation))
      print(x)
      tmp_css_colors<- c()
      for(i in 1:x)
      {
        tmp_css_colors<- c(tmp_css_colors, css_colors[i])
      }
      datatable(annotation, extensions = 'Scroller', options = list( rownames=T,deferRender = TRUE, scrollY = 200, scroller = TRUE, rowCallback = JS(rowCallback_generated))) 
    })
      
    expression_colors_pies <- T
    output$tabVizPie_charts<-renderUI({
      s = input$chooseGroups2_rows_selected
      
      if(input$expressions_pies ==T){
            expression_colors_pies = T
          }
          else if(input$expressions_pies == F){
            expression_colors_pies = F
          }
        
      source("interactive_pie_charts.R", local = T)
      lay <- input$layouts2
      pie_charts()
      tags$iframe(
        srcdoc = paste(readLines('output2.html'), collapse = '\n'),
        width = "100%",
        height = "600px")
    })
    
    label_selected_pies <- T
    some_labels_pies <- T
    # output$tabVizPie_charts <- renderPlot({
    #   s = input$chooseGroups2_rows_selected
    #   if(input$expressions_pies ==T){
    #     expression_colors_pies = T
    #   }
    #   else if(input$expressions_pies == F){
    #     expression_colors_pies = F
    #   }
    #   if (input$labels_pies == T){
    #     label_selected_pies = T
    #   }
    #   else  if (input$labels_pies == F){
    #     label_selected_pies = F 
    #   }
    #   if (input$some_labels_pies == T){
    #     some_labels_pies = T
    #   }
    #   else  if (input$some_labels_pies == F){
    #     some_labels_pies = F 
    #   }
    #   lay <- input$layouts2
    #   source("pie_charts(stableInput).R", local = T)
    #   pie_chartsInput()
    # })
    
    output$pie_chartsDownload = downloadHandler(
      filename = 'pie-charts.png',
      content = function(file) {
        source("pie_charts(stableInput).R", local = T)
        png(file)
        print(pie_chartsInput())
        dev.off()
      })
    
    
    ### Convex Hull ###
    
    output$chooseGroups <- DT::renderDataTable({
      annotation<- fetchFirstSelectedStoredDataset2()
      if (is.null(annotation)) 
        annotation<- EmptyDataset(c("Annotations", "Nodes"))
      rowCallback_generated <-"function(row, dat, displayNum, index){"
      for (i in 1: length(rownames(annotation)))
      {
        rowCallback_generated<-paste(rowCallback_generated, "if(dat[0]==",i,"){","$('td:eq(1)', row).addClass('x",i,"');}" ,sep="")
      }
      rowCallback_generated<-paste(rowCallback_generated, "}",sep="")
      
      css_colors <- group_pal_rows(length(rownames(annotation)))
      ########
      
      x<- length(rownames(annotation))
      tmp_css_colors<- c()
      for(i in 1:x)
      {
        tmp_css_colors<- c(tmp_css_colors, css_colors[i])
      }
      datatable(annotation, extensions = 'Scroller', options = list( rownames=T,deferRender = TRUE, scrollY = 200, scroller = TRUE, rowCallback = JS(rowCallback_generated))) 
      })
    
    ####### HTML trials ########
    expression_colors <- T
    
    output$interactive_convex_hulls<-renderUI({
      s = input$chooseGroups_rows_selected
      
      if(input$expressions ==T){
            expression_colors = T
            }
          else if(input$expressions == F){
            expression_colors = F
            }
        
      source("interactive_convex_hulls.R", local = T)
      lay <- input$layouts
      convex_hulls()
      tags$iframe(
        srcdoc = paste(readLines('output.html'), collapse = '\n'),
        width = "100%",
        height = "600px")
      })
    
####################################
    label_selected <- T
    some_labels<- T
    # output$tabVizConvex_hull <- renderPlot({
    #   s = input$chooseGroups_rows_selected
    #   if(input$expressions ==T){
    #     expression_colors = T
    #     }
    #   else if(input$expressions == F){
    #     expression_colors = F
    #     }
    #   if (input$labels == T){
    #     label_selected = T
    #   }
    #   else  if (input$labels == F){
    #     label_selected = F 
    #   }
    #   if (input$some_labels == T){
    #     some_labels = T
    #   }
    #   else  if (input$some_labels == F){
    #     some_labels = F 
    #   }
    #   lay <- input$layouts
    #     source("convex_hullInput.R", local = T)
    #     convexInput()
    # })
   
    
    output$convexDownload = downloadHandler(
      filename = 'convex_hull.png',
      content = function(file) {
        source("download_convexhull.R", local = T)
        png(file)
        print(download_convexhull())
        dev.off()
      })
    
    
    #######################################################################
    #Expression
    ## Fetch etc ##
    
    getStoredExpressionChoices <- function() {
      sexpress <- StoredExpress()
      if (nrow(sexpress) == 0) 
        return(NULL)
      choices <- sexpress$id
      names(choices) <- sexpress$name
      return(choices)
    }
    
    StoredExpressEmpty <- function() {
      return(nrow(reactiveVars$StoredExpressions) == 0)
    }
    
    
    StoredExpress <- reactive({
      return(reactiveVars$StoredExpressions)
    })
    
    getDatasetNameEx <- function(id) {
      sn <- StoredExpress()
      idi <- which(sn$id == id)
      if (is.null(sn) || nrow(sn) == 0 || length(idi) == 0) 
        return(NULL)
      return(sn[idi, ]$name)
    }
    
    
    
    
    
    SelectedStoredExpress <- function() {
      if (length(reactiveVars$SelectedStoredExpressionIds) > 0) 
        return(StoredExpress()[which(reactiveVars$StoredExpressions$id %in% 
                                       reactiveVars$SelectedStoredExpressionIds), ]) else if (nrow(StoredAnnots()) == 0 || is.na(StoredAnnots()[1, ])) 
                                         return(NULL) else {
                                           updateCheckboxGroupInput(session, "storedGraphsOutputMultipleSelectTopolopgy3", "Selected expression(s)", choices = getStoredExpressionChoices(), selected = getStoredExpressionChoices()[1])
                                           return(StoredAnnots()[1, ])
                                         }
    }
    
    fetchFirstSelectedStoredExpression <- reactive({
      sexpress <- SelectedStoredExpress()
      if (!is.null(sexpress) && nrow(sexpress) > 0) {
        return(fetchDatasetEx(sexpress[1, ]$id))
      } else {
        return(NULL)
      }
    })
    
    fetchDatasetEx <- function(nid) {
      retVal <- NULL
      if (length(nid) > 0) {
        retVal <- readRDS(paste0(nid, ".rda"))
        attr(retVal, "id") <- nid
      }
      return(retVal)
    }
    
    fetchFirstSelectedStoredDatasetEx <- reactive({
      sexpress <- SelectedStoredExpress()
      if (!is.null(sexpress) && nrow(sexpress) > 0) {
        return(fetchDatasetEx(sexpress[1, ]$id))
      } else {
        return(NULL)
      }
    })
    
    fetchFirstSelectedStoredGroupsEx <- reactive({
      sexpress <- SelectedStoredExpress()
      if (!is.null(sexpress) && nrow(sexpress) > 0) {
        return(fetchDatasetEx(sexpress[1, ]$id))
      } else {
        return(NULL)
      }
    })
    
    
    
    # Read files
    loadExpressions <- function() {
      expression1 <- NULL
      switch(input$uiLoadExpressions, oF = {
        if (!is.null(input$file3)) {
          expression1 <- read_expressions(input$file3$datapath)
        }
      }, oR_Expression_file = {
        expression1 <- read.delim("C:/Users/mikae/Desktop/MSc_Molecular_Biomedicine/Master_Thesis_Pavlopoulos/NAP/NORMA_project/string_expression_colors.txt", header= F)
      })
   
      colnames(expression1) <- c("ID", "Color")
      
      # if (input$uiLoadExpressions != "oF" && !is.null(expression1)) {
      #   if (!is.null(expression1))
      #     expression1 <- EmptyDataset(c("Nodes", "Color"))
      #   # colnames(expression1) <- c("Nodes", "Color")
      # }
      # 
      return(expression1)
    }

    output$uiLoadExpressions <- renderUI({
      if (is.null(input$uiLoadExpressions))
        return()
      
      # Depending on input$input_type, we'll generate a different UI
      # component and send it to the client.
      if (input$uiLoadExpressions == "oF") {
        wellPanel(fileInput("file3", "Choose file to upload", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")))
      }
    })
    
    doAddExpression <- observeEvent(input$btnAddExpression, {
      expression<- loadExpressions()
      if (!is.null(expression)) {
        nid <- UUIDgenerate(T)      #time-base UUID is generated
        nn <- input$expressionName 
        cnt <- 1                    #count
        while (nn %in% reactiveVars$StoredExpressions$name) {  #reactiveVars: represents a single reactive variable.
          cnt <- cnt + 1
          nn <- paste(input$expressionName, cnt)      #paste: converts its arguments (via as.character) to character strings
        }
        dtf <- data.frame(id = nid, name = nn, stringsAsFactors = F)
        if (nrow(expression) > 10000) {
          expressionName<- expressionName[1:10000, ]
        }
        reactiveVars$StoredExpressions <- rbind(reactiveVars$StoredExpressions, dtf)
        saveRDS(expression, paste0(nid, ".rda"))
        if (length(reactiveVars$SelectedStoredExpressionIds) == 0) {
          reactiveVars$SelectedStoredExpressionIds <- c(nid)
        }
      } else return()
    })
    
    
    doRemExpression<- observeEvent(input$btnRemoveExpression, {
      if (!is.null(input$availableExpressions)) {
        reactiveVars$StoredExpressions <- reactiveVars$StoredExpressions[-which(reactiveVars$StoredExpressions$id %in% 
                                                                                  input$availableExpressions), ]
        nr <- nrow(reactiveVars$StoredExpressions)
        if (nr > 0) 
          reactiveVars$SelectedStoredExpressionIds <- reactiveVars$StoredExpressions[nr, ]$id
      }
    })
    
    uploadTabSetSelectedExpression <- observeEvent(input$uiStoredGraphsOutputSelectUploadExpressions, {
      reactiveVars$SelectedStoredExpressionIds <- c(input$uiStoredGraphsOutputSelectUploadExpressions)
    }, ignoreNULL = FALSE)
    
    output$uiStoredGraphsOutputRadioEx <- renderUI({
      input$btnAddExpression
      input$btnRemoveExpression
      choices <- getStoredExpressionChoices()
      if (is.null(choices)) 
        return()
      return(list(br(), wellPanel(h4("Available Expressions"), checkboxGroupInput("availableExpressions", label = "", choices = choices)), div(div(actionButton("btnRemoveExpression", "REMOVE", icon = icon("minus")), class = "col-md-5 centerBlock"), class = "row text-center")))
      
    })
    
    output$uiStoredGraphsOutputSelectUploadExpressions <- renderUI({
      input$btnAddExpression
      input$btnRemoveExpression
      choices <- getStoredExpressionChoices()
      if (is.null(choices)) 
        return()
      return(selectInput("uiStoredGraphsOutputSelectUploadExpressions", "Selected Expression", choices))
    })
 
    
    
    
    
    
    
    
    
  
  
    
    
    
    ######## Basic Info ################
    output$info_vertices1 <- renderText({
        igraph <- fetchFirstSelectedStoredIgraph()
        if (is.null(igraph)) 
            return()
        igraphName <- SelectedStoredNets()$name
        paste("Number of nodes in", igraphName, ":", vcount(igraph))
    })
    output$info_edges1 <- renderText({
        igraph <- fetchFirstSelectedStoredIgraph()
        if (is.null(igraph)) 
            return()
        igraphName <- SelectedStoredNets()$name
        paste("Number of intercations in", igraphName, ":", ecount(igraph))
    })
    
    
    ############################################## 
    
    output$vertices1_label <- renderText({
        igraph <- fetchFirstSelectedStoredIgraph()
        if (is.null(igraph)) 
            return()
        igraphName <- SelectedStoredNets()$name
        paste("Number of vertices in", igraphName, ":", vcount(igraph))
    })
    
    # Print the vertices
    output$vertices1 <- renderPrint({
        igraph <- fetchFirstSelectedStoredIgraph()
        if (is.null(igraph)) 
            return(invisible(""))
        cat(sort(V(igraph)$name, decreasing = FALSE), sep = "\t")
    })
    
    output$edges1_label <- renderText({
        igraph <- fetchFirstSelectedStoredIgraph()
        if (is.null(igraph)) 
            return()
        igraphName <- SelectedStoredNets()$name
        paste("Number of edges in", igraphName, ":", ecount(igraph))
    })
    
    output$edges1 <- renderPrint({
        igraph <- fetchFirstSelectedStoredIgraph()
        if (is.null(igraph)) 
            return(invisible(""))
        # if (as.logical(is.directed(igraph))) {
        #     edgesym <- "<U+2794>"
        # } 
        else {
            edgesym <- "<U+2015>"
        }
        
        cat(gsub("\\|", edgesym, attr(E(igraph), "vnames")), sep = "\t")
        # get.edgelist(igraph) get.data.frame(igraph, what=c( 'edges'))
    })
    
    
    
    ################################################# 
    output$diameter1 <- renderText({
        igraph <- fetchFirstSelectedStoredIgraph()
        if (is.null(igraph)) 
            return()
        diameter <- diameter(igraph)
        paste("Diameter of Graph 1:", diameter)
        # unique_vertices <- length(vertices)
    })
    
    
    ################################################ https://rstudio.github.io/DT/shiny.html
    ################################################ http://rstudio.github.io/DT/functions.html
    ################################################ https://cran.r-project.org/web/packages/DT/DT.pdf Print the data in
    ################################################ Table format ####################
    
    output$datasettab1 <- DT::renderDataTable({
        dataset <- fetchFirstSelectedStoredDataset()
        if (is.null(dataset)) 
            dataset <- EmptyDataset(c("Source", "Target", "Weight"))
        return(datatable(dataset, rownames = FALSE, editable = F) %>% formatStyle(colnames(dataset), fontSize = ui_options["ui_table_font_sz"]) %>% formatRound("Weight"))
    
    })

    
    ################ Statistics ########
    stat_dataset <- function(dataset, datasetName) {
      res <- tryCatch({
        columns <- c("Statistic", paste0("Value for ", datasetName))
        if (is.null(dataset)) {
          dataset <- EmptyDataset(columns)
        } else {
          igraph <- convert_to_igraph(dataset)
          if (length(input$statistics) == 0) {
            dataset <- EmptyDataset(columns)
          } else {
            dataset <- netstats(igraph, input$statistics)
            colnames(dataset) <- columns
          }
        }
        return(dataset)
      }, warning = function(w) {
        
      }, error = function(e) {
        cat(paste0("ERROR while executing stat_dataset: ", e, "\n"))
        return(NULL)
      }, finally = {
        
      })
      return(res)
    }  
    
    stat_dataset2 <- function(dataset, datasetName) {
      res <- tryCatch({
        columns <- c("Statistic", paste0("Value for ", datasetName))
        if (is.null(dataset)) {
          dataset <- EmptyDataset(columns)
        } else {
          igraph <- convert_to_igraph(dataset)
          if (length(input$statistics2) == 0) {
            dataset <- EmptyDataset(columns)
          } else {
            dataset <- netstats(igraph, input$statistics2)
            colnames(dataset) <- columns
          }
        }
        return(dataset)
      }, warning = function(w) {
        
      }, error = function(e) {
        cat(paste0("ERROR while executing stat_dataset: ", e, "\n"))
        return(NULL)
      }, finally = {
        
      })
      return(res)
    }
    output$statres <- DT::renderDataTable({
      dset <- fetchFirstSelectedStoredDataset()
      stat_dataset2(dset, getDatasetName(attr(dset, "id")))
    }, options = list(paging = FALSE, dom = "t", rowCallback = JS("function(row, data) {", "var num = parseFloat(data[2]).toFixed(2);", "if(isNaN(num)){num = data[2];}", "$('td:eq(2)', row).html(num);", "}")))
    
    doRefreshPalette <- observeEvent(input$btnRefreshPalette, {
        ncolors <- length(colors)
        colors <<- randomColor(ncolors)
    })
    
    output$statisticsMethodsBarPlot <- renderPlot({
        input$btnRefreshPalette
        datasets <- fetchAllSelectedStoredDataset()
        if (length(datasets) == 0) 
            return()
        stat_res <- NULL
        for (dataset_i in 1:length(datasets)) {
            dataset <- datasets[[dataset_i]]
            datasetName <- getDatasetName(names(datasets)[dataset_i])
            dataset_stat_res <- stat_dataset(dataset, "")
            if (is.null(dataset_stat_res) || nrow(dataset_stat_res) == 
                0) 
                next
            if (is.null(stat_res)) {
                stat_res <- dataset_stat_res
                stat_res$network <- datasetName
            } else {
                dataset_stat_res$network <- datasetName
                stat_res <- rbind(stat_res, dataset_stat_res)
            }
        }
        if (is.null(stat_res)) 
            return()
        colnames(stat_res) <- c("statistic", "value", "network")
        stat_res$value <- as.numeric(as.character(stat_res$value))
        stat_res$network <- factor(stat_res$network)
        ncolors <- length(levels(stat_res$network))
        if (length(colors) < ncolors) 
            colors <<- randomColor(ncolors)
        plot.settings <- list(superpose.polygon = list(col = colors[1:ncolors], border = "transparent"), strip.border = list(col = "black"))
        barchart(value ~ statistic, group = network, data = stat_res, origin = 0, horizontal = F, auto.key = list(space = "right", points = F, rectangles = T), par.settings = plot.settings, scales = list(x = list(rot = 45)), panel = function(x, y, ...) {
                panel.grid(h = -1, v = 0, col = "gray")
                panel.barchart(x, y, ...)
            })
    })
    
    output$statisticsMethodsPlotRender <- renderUI({
        plotOutput("statisticsMethodsBarPlot", height = paste0(as.integer(480 + 
            480 * (input$statisticsPlotPercentMagnify/100)), "px"))
    })
    
    doStatSelectAll <- observeEvent(input$btnStatSelectAll, {
        updateCheckboxGroupInput(session, "statistics2", selected = statistics)
    })
    
    doStatSelectNone <- observeEvent(input$btnStatSelectNone, {
        updateCheckboxGroupInput(session, "statistics2", selected = character(0))
    })
    
    
  
    
})