reactiveVars$StoredNetworks2_annotations_tab <- data.frame(id = character(), name = character(), stringsAsFactors = F)
reactiveVars$SelectedStoredNetworksIds2_annotations_tab <- c()

fetchFirstSelectedStoredDataset2_annotations_tab <- reactive({
  ssn <- SelectedStoredNets2_annotations_tab()
  # print("---")
  # print(ssn)
  if (!is.null(ssn) && nrow(ssn) > 0) {
    return(fetchDataset2_annotations_tab(ssn[1, ]$id))
  } else {
    return(NULL)
  }
})

fetchFirstSelectedStoredAnnotations2_annotations_tab <- function() {
  groups_ann <- fetchFirstSelectedStoredGroups2_annotations_tab()
  if (is.null(groups_ann)) 
    return(NULL) else return(groups_ann)
}

fetchFirstSelectedStoredGroups2_annotations_tab <- reactive({
  sannots <- SelectedStoredNets2_annotations_tab()
  if (!is.null(sannots) && nrow(sannots) > 0) {
    return(fetchDataset2_annotations_tab(sannots[1, ]$id))
  } else {
    return(NULL)
  }
})
# fetchFirstSelectedStoredIgraph2_annotations_tab <- function() {
#   dataset <- fetchFirstSelectedStoredDataset2_annotations_tab()
#   # print(dataset)
#   if (is.null(dataset))
#     return(NULL) else return(convert_to_igraph(dataset))
# }

fetchDataset2_annotations_tab <- function(nid) {
  retVal <- NULL
  if (length(nid) > 0) {
    retVal <- readRDS(paste0(nid, ".rda"))
    attr(retVal, "id") <- nid
  }
  return(retVal)
}



fetchAllSelectedStoredDataset2_annotations_tab <- function() {
  ssn <- SelectedStoredNets2_annotations_tab()
  ids <- c()
  if (!is.null(ssn) && nrow(ssn) > 0) {
    ret <- list()
    for (i in 1:nrow(ssn)) {
      ret[[i]] <- fetchDataset2_annotations_tab(ssn[i, ]$id)
      ids <- c(ids, ssn[i, ]$id)
    }
    names(ret) <- ids
    return(ret)
  } else {
    return(NULL)
  }
}


fetchMaxNSelectedStoredDataset2_annotations_tab <- function(N) {
  ssn <- SelectedStoredNets2_annotations_tab()
  if (!is.null(ssn) && nrow(ssn) > 0) {
    ret <- list()
    for (i in 1:nrow(ssn)) {
      if (i > N) 
        break
      ret[[i]] <- fetchDataset2_annotations_tab(ssn[i, ]$id)
    }
    return(ret)
  } else {
    return(NULL)
  }
}

# output$tabVizIgraphSimple<- renderVisNetwork({
#   g <- fetchFirstSelectedStoredIgraph2_annotations_tab()
#   
  
  
  output$uiLoadGraphOptionsOutput2_annotations_tab <-  renderUI({
    input$btnAddNetwork2
    input$btnRemoveNetworks2
    choices <- getStoredNetsChoices2_annotations_tab()
    if (is.null(choices))
      return()
    return(selectInput("storedGraphsOutputSelectUpload2_annotations_tab", "Selected network", choices))
    
  })
  
  
  uploadTabSetSelectedNetwork2_annotations_tab <- observeEvent(input$storedGraphsOutputSelectUpload2_annotations_tab, {
    reactiveVars$SelectedStoredNetworksIds2_annotations_tab <- c(input$storedGraphsOutputSelectUpload2_annotations_tab)
  }, ignoreNULL = FALSE)
  
  
  
  
  getStoredNetsChoices2_annotations_tab <- function() {
    snets <- StoredNets2_annotations_tab()
    if (nrow(snets) == 0) 
      return(NULL)
    choices <- snets$id
    names(choices) <- snets$name
    return(choices)
  }
  
  StoredNets2_annotations_tab <- reactive({
    return(reactiveVars$StoredNetworks2_annotations_tab)
  })
  StoredNetsEmpty2_annotations_tab <- function() {
    return(nrow(reactiveVars$StoredNetworks2_annotations_tab) == 0)
  }
  
  SelectedStoredNets2_annotations_tab <- function() {
    if (length(reactiveVars$SelectedStoredNetworksIds2_annotations_tab) > 0) {
      # print(StoredNetworks2_annotations_tab()[which(reactiveVars$StoredNetworks2_annotations_tab$id %in% reactiveVars$SelectedStoredNetworksIds2_annotations_tab), ])
      return(StoredNets2_annotations_tab()[which(reactiveVars$StoredNetworks2_annotations_tab$id %in% 
                                               reactiveVars$SelectedStoredNetworksIds2_annotations_tab), ])} 
    else if (nrow(StoredNets2_annotations_tab()) == 0 || is.na(StoredNets2_annotations_tab()[1, ])) 
      return(NULL) else {
        updateCheckboxGroupInput(session, "uiLoadGraphOptionsOutput2_annotations_tab", "Selected network(s)", choices = getStoredNetsChoices2_annotations_tab(), selected = getStoredNetsChoices2_annotations_tab()[1])
        return(StoredNets2_annotations_tab()[1, ])
      }
  }
  
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
      
      
      saveRDS(dataset, paste0(nid, ".rda"))
      if (length(reactiveVars$SelectedStoredNetworksIds2_annotations_tab) == 0) {
        reactiveVars$SelectedStoredNetworksIds2_annotations_tab <- c(nid)
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
  