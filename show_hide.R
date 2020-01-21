source("convex_hullInput.R")

leaflet() %>%
  addTiles() %>%
  addMarkers(data = members_with_NA_groups, group = members_with_NA_groups$group) 



moduleTestUI <- function(id){
  
  source("convex_hullInput.R")
  
  ns <- NS(id)
  
  tagList(
    
    disabled(div(

      id=ns('Groups'),
      
      # textInput(ns('text'), 'text input'),
      selectInput(ns('select'), 'Groups', choices = c("Groups"))
      # checkboxInput(ns('checkbox'), 'checkbox'),
      # radioButtons(ns('radio'), 'radiobuttons', LETTERS[1:5], inline=T),
      # sliderInput(ns('slider'), 'slider', 1, 5, 1),
      # actionButton(ns('button'), 'button')
      
    )),
    
    hr(),
    
    actionButton(ns('show'), 'Show'),
    actionButton(ns('hide'), 'Hide')
    # actionButton(ns('enable'), 'Enable'),
    # actionButton(ns('disable'), 'Disable'),
    # actionButton(ns('reset'), 'Reset')
    
  )
}

moduleTest <- function(input, output, session){
  
  observeEvent(input$show, {
  annotation<- fetchFirstSelectedStoredDataset2()
  if (is.null(annotation)) 
    annotation<- EmptyDataset(c("Annotations", "Nodes"))
  return(datatable(annotation, rownames = FALSE) %>% formatStyle(colnames(annotation), fontSize = ui_options["ui_table_font_sz"]))
  
  
  onclick('show', show('Groups'))
  onclick('hide', hide('Groups'))
  # onclick('enable', enable('the_div'))
  # onclick('disable', disable('the_div'))
  # onclick('reset', reset('the_div'))
  # 
})}