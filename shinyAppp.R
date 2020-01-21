# 
# 
# datasetInput <- reactive({
#   print(fetchAllSelectedStoredDataset()[1])
#   dset <- fetchFirstSelectedStoredDataset()
#   #print(fetchAllSelectedStoredDataset())
#   #print(fetchFirstSelectedStoredDataset())
#   #print(dset)
#   if(is.null(dset))
#     return(NULL)
#   anifile <- stat_dataset(dset, F)
#   anifile <- as.data.frame(anifile)
#   # print(anifile)
# })
# 
# output$choose_columns <- renderUI({
#   if(is.null(datasetInput()))
#     return()
#   colnames <- names(datasetInput())
#   # updateCheckboxGroupInput(session, "storedGraphsOutputMultipleSelectTopolopgy", "Selected network(s)", choices = getStoredNetsChoices(), selected = getStoredNetsChoices()[1])
#   # return(StoredNets()[1, ])
#   checkboxGroupInput("columns", "Choose Columns",
#                      choices  = colnames,
#                      selected = colnames)
# })
# output$statres = DT::renderDataTable({
#   if(is.null(datasetInput()))
#     return()
#   if(is.null(input$columns) || !(input$columns %in% names(datasetInput())))
#     return()
#   {datatable(datasetInput()[, input$columns, drop = F] ,filter = "top")}
#   
# })
# 
# 
# 

# output$statres <- DT::renderDataTable({
#     dset <- fetchFirstSelectedStoredDataset()
#     stat_dataset(dset, getDatasetName(attr(dset, "id")), F)
# }, options = list(paging = FALSE, lengthChange = TRUE, dom = "t", rowCallback = JS("function(row, data) {", "var num = parseFloat(data[2]).toFixed(2);", "if(isNaN(num)){num = data[2];}", "$('td:eq(2)', row).html(num);", "}")))



# output$statres <- DT::renderDataTable({
#     dset <- StoredNets()
#     # print(fetchFirstSelectedStoredDataset())
#     columns<- c()
#     for (i in 1:length(StoredNets())){
#       # stat_dataset_i <- stat_dataset(dset, getDatasetName(attr(dset, "id")), F)
#       # columns <- c(columns, stat_dataset_i)
#       
#       columns_i <- c("Statistic", paste0("Value for ", StoredNets()$name[[i]]))
#       columns <- c(columns, columns_i)
#       columns<- columns[!duplicated(columns)]
#       #colnames(dset)<- columns
#     }
#     print(columns)
#     print(dset)
#     print(dim(dset))
#     
#     
#     
#     if (is.null(dset)) 
#       dset<- EmptyDataset(c("Statistic"))
#     datatable(dset, rownames = FALSE, extensions = 'Responsive') %>% formatStyle(colnames(dset), fontSize = ui_options["ui_table_font_sz"])
# })

# output$statres <- DT::renderDataTable({
# # datasets <- fetchAllSelectedStoredDataset()
# datasets <- fetchFirstSelectedStoredDataset()
# # print(length(fetchAllSelectedStoredDataset()))
# if (length(datasets) == 0)
#   return()
# stat_res <- NULL
# stat_res<- c()
# for (dataset_i in 1:length(datasets)) {
#   dataset <- datasets[[dataset_i]]
#   datasetName <- getDatasetName(names(datasets)[dataset_i])
#   dataset_stat_res <- stat_dataset(datasets, getDatasetName(attr(datasets, "id")), F)
#   # print(dataset_stat_res)
#   # if (is.null(dataset_stat_res) || nrow(dataset_stat_res) ==
#   #     0)
#   #   next
#   # if (is.null(stat_res)) {
#   #   stat_res <- dataset_stat_res
#   #   stat_res$network <- datasetName
#   #   print(stat_res)
#   # } else {
#   #   dataset_stat_res$network <- datasetName
#   #   stat_res <- rbind(stat_res, dataset_stat_res)
#   # }
#   # stat_res <- c(stat_res,dataset_stat_res)
#   # print(stat_res)
#   # print(dataset_stat_res)
#   
#   stat_res <- c(stat_res,dataset_stat_res)
#   print(stat_res)
# }
# dataset_stat_res
# 
# # if (is.null(stat_res))
# #   return()
# # # colnames(stat_res) <- c("statistic", "value", "network")
# # # stat_res$value <- as.numeric(as.character(stat_res$value))
# # stat_res$network <- factor(stat_res$network)
# 
# }, options = list(paging = FALSE, lengthChange = TRUE, dom = "t", rowCallback = JS("function(row, data) {", "var num = parseFloat(data[2]).toFixed(2);", "if(isNaN(num)){num = data[2];}", "$('td:eq(2)', row).html(num);", "}")
#                   ))

# stat_dataset[, paste("Value for ", StoredNets()$name[[1]]) := statistics] 
# cn <- parse(text = StoredNets()$name[[1]])
# stat_dataset[, (cn) := statistics, by = id]
# head(statres)

# # store a proxy of tbl 
# proxy <- dataTableProxy(outputId = "statres")
# 
# # each time addData is pressed, add user_table to proxy
# observeEvent(eventExpr = input$availableNetworks, {
#   proxy %>% 
#     addRow(user_table)
# })





library(shiny)
library(shinyjs)
library(DT)
library(dplyr)
library(data.table)


ui = pageWithSidebar(
  
  headerPanel(""),
  
  #This is where the full animal information file is input, as a ".txt" file.
  sidebarPanel(uiOutput("choose_columns"), 
               width=2),
  mainPanel(
    DT::dataTableOutput("ani1")
  ))



server = function(input, output, session) {
  
  
  datasetInput <- reactive({
    DT = data.table(
      ID = c("b","b","b","a","a","c"),
      a = 1:6,
      b = 7:12,
      c = 13:18
    )
    if(is.null(DT))
      return(NULL)
    anifile <- as.data.frame(DT)    #This removes the Ewes and Status non-zero Rams from the displayed data, so that only live/at hand Rams are shown for selection.    
  })
  
 
  
  
  
  
  
  output$choose_columns <- renderUI({
    if(is.null(datasetInput()))
      return()
    colnames <- names(datasetInput())
    
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("columns", "Choose Columns", 
                       choices  = colnames,
                       selected = colnames)
  })
  
 
  #This line is repsonsible for creating the table for display.
  output$ani1 = DT::renderDataTable({
    if(is.null(datasetInput()))
      return()
    if(is.null(input$columns) || !(input$columns %in% names(datasetInput())))
      return()
    {datatable(datasetInput()[, input$columns, drop = F] ,filter = "top")}
  })

}

shinyApp(ui=ui, server=server)