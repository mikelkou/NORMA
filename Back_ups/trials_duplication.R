reactiveVars$StoredNetworks_topology_tab <- data.frame(id = character(), name = character(), stringsAsFactors = F)
reactiveVars$SelectedStoredNetworksIds_topology_tab <- c()

fetchFirstSelectedStoredDataset_topology_tab <- reactive({
  ssn <- SelectedStoredNets_topology_tab()
  if (!is.null(ssn) && nrow(ssn) > 0) {
    return(fetchDataset_topology_tab(ssn[1, ]$id))
  } else {
    return(NULL)
  }
})

fetchFirstSelectedStoredStoredNetworks_topology_tab <- function() {
  topol_nets <- fetchFirstSelectedStoredStoredNetworks_topology_tab2()
  if (is.null(topol_nets)) 
    return(NULL) else return(topol_nets)
}

fetchFirstSelectedStoredStoredNetworks_topology_tab2 <- reactive({
  stopol <- SelectedStoredNets_topology_tab()
  if (!is.null(stopol) && nrow(stopol) > 0) {
    return(fetchDataset_topology_tab(stopol[1, ]$id))
  } else {
    return(NULL)
  }
})

fetchFirstSelectedStoredIgraph_topology_tab <- function() {
  dataset <- fetchFirstSelectedStoredDataset_topology_tab()
  # print(dataset)
  if (is.null(dataset))
    return(NULL) else return(convert_to_igraph(dataset))
}

fetchDataset_topology_tab <- function(nid) {
  retVal <- NULL
  if (length(nid) > 0) {
    retVal <- readRDS(paste0(nid, ".rda"))
    attr(retVal, "id") <- nid
  }
  return(retVal)
}



fetchAllSelectedStoredDataset_topology_tab <- function() {
  ssn <- SelectedStoredNets_topology_tab()
  ids <- c()
  if (!is.null(ssn) && nrow(ssn) > 0) {
    ret <- list()
    for (i in 1:nrow(ssn)) {
      ret[[i]] <- fetchDataset_topology_tab(ssn[i, ]$id)
      ids <- c(ids, ssn[i, ]$id)
    }
    names(ret) <- ids
    return(ret)
  } else {
    return(NULL)
  }
}


fetchMaxNSelectedStoredDataset_topology_tab <- function(N) {
  ssn <- SelectedStoredNets_topology_tab()
  if (!is.null(ssn) && nrow(ssn) > 0) {
    ret <- list()
    for (i in 1:nrow(ssn)) {
      if (i > N) 
        break
      ret[[i]] <- fetchDataset_topology_tab(ssn[i, ]$id)
    }
    return(ret)
  } else {
    return(NULL)
  }
}


output$uiStoredGraphsOutputSelectTopolopgy <- renderUI({
  input$btnAddNetwork
  input$btnRemoveNetworks
  choices <- getStoredNetsChoices_topology_tab()
  if (is.null(choices)) 
    return()
  return(selectInput("storedGraphsOutputSelectTopolopgy", "Selected network", choices))
})  
  
  
  # output$uiLoadGraphOptionsOutput2_annotations_tab <-  renderUI({
  #   input$btnAddNetwork2
  #   input$btnRemoveNetworks2
  #   choices <- getStoredNetsChoices2_annotations_tab()
  #   if (is.null(choices))
  #     return()
  #   return(selectInput("storedGraphsOutputSelectUpload2_annotations_tab", "Selected network", choices))
  #   
  # })
  
  
  # uploadTabSetSelectedNetwork2_annotations_tab <- observeEvent(input$storedGraphsOutputSelectUpload2_annotations_tab, {
  #   reactiveVars$SelectedStoredNetworksIds2_annotations_tab <- c(input$storedGraphsOutputSelectUpload2_annotations_tab)
  # }, ignoreNULL = FALSE)
  # 
  # 
  
  
  getStoredNetsChoices_topology_tab <- function() {
    snets <- StoredNets_topology_tab()
    if (nrow(snets) == 0) 
      return(NULL)
    choices <- snets$id
    names(choices) <- snets$name
    return(choices)
  }
  
  StoredNets_topology_tab <- reactive({
    return(reactiveVars$StoredNetworks_topology_tab)
  })
  StoredNetsEmpty_topology_tab <- function() {
    return(nrow(reactiveVars$StoredNetworks_topology_tab) == 0)
  }
  
  SelectedStoredNets_topology_tab <- function() {
    if (length(reactiveVars$SelectedStoredNetworksIds_topology_tab) > 0) {
      # print(StoredNetworks_topology_tab()[which(reactiveVars$StoredNetworks_topology_tab$id %in% reactiveVars$SelectedStoredNetworksIds_topology_tab), ])
      return(StoredNets_topology_tab()[which(reactiveVars$StoredNetworks_topology_tab$id %in% 
                                               reactiveVars$SelectedStoredNetworksIds_topology_tab), ])} 
    else if (nrow(StoredNets_topology_tab()) == 0 || is.na(StoredNets_topology_tab()[1, ])) 
      return(NULL) else {
        updateCheckboxGroupInput(session, "uiStoredGraphsOutputSelectTopolopgy", "Selected network(s)", choices = getStoredNetsChoices_topology_tab(), selected = getStoredNetsChoices_topology_tab()[1])
        return(StoredNets_topology_tab()[1, ])
      }
  }
  # updateCheckboxGroupInput(session, "storedGraphsOutputMultipleSelectTopolopgy", "Selected network(s)", choices = getStoredNetsChoices(), selected = getStoredNetsChoices()[1])
  
  doAddNetwork <- observeEvent(input$btnAddNetwork, {
    dataset <- loadNetworkFromFile()
    if (!is.null(dataset)) {
      nid <- UUIDgenerate(T)      #time-base UUID is generated
      nn <- input$networkName 
      cnt <- 1                    #count
      while (nn %in% reactiveVars$StoredNetworks2_annotations_tab$name) {  #reactiveVars: represents a single reactive variable. 
        cnt <- cnt + 1
        nn <- paste(input$networkName, cnt)      #paste: converts its arguments (via as.character) to character strings
      }
      df <- data.frame(id = nid, name = nn, stringsAsFactors = F)
      if (nrow(dataset) > 10000) {
        dataset <- dataset[1:10000, ]
      }
      # attr(dataset, which = "directed") <- input$directed1
      # attr(dataset, which = 'weighted') <- input$weighted1
      
      reactiveVars$StoredNetworks2_annotations_tab <- rbind(reactiveVars$StoredNetworks2_annotations_tab, df)
      
      reactiveVars$StoredNetworks_just_network <- reactiveVars$StoredNetworks
      
      reactiveVars$StoredNetworks_topology_tab <- reactiveVars$StoredNetworks
      
      
      saveRDS(dataset, paste0(nid, ".rda"))
      if (length(reactiveVars$SelectedStoredNetworksIds_topology_tab) == 0) {
        reactiveVars$SelectedStoredNetworksIds_topology_tab <- c(nid)
      }
    } else createAlert(session, "tabUploadSideAlert", "fileUploadAlert", title = "ERROR !", style = "danger", content = paste0("An error occurred while trying to read your file. Please make sure that it is formatted according to the requirements."), append = FALSE)
  })
  

  doRemNetwork <- observeEvent(input$btnRemoveNetworks, {
    if (!is.null(input$availableNetworks)) {
      reactiveVars$StoredNetworks <- reactiveVars$StoredNetworks[-which(reactiveVars$StoredNetworks$name %in% 
                                                                          input$availableNetworks), ]
      nr <- nrow(reactiveVars$StoredNetworks)
      if (nr > 0) 
        reactiveVars$SelectedStoredNetworksIds <- reactiveVars$StoredNetworks[nr, ]$id
    }
  })
  