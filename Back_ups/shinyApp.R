library(shiny)
library(DT)
library(RColorBrewer)


colors <- c('#e34755', '#99d8c9', '#9900c9', '#955822', '#ffdaa9' )
rowCallback_generated <-"function(row, dat, displayNum, index){"
for (i in 1: length(colors))
{
  rowCallback_generated<-paste(rowCallback_generated, "if(dat[0]==",i,"){","$('td:eq(1)', row).addClass('x",i,"');}" ,sep="")
}
rowCallback_generated<-paste(rowCallback_generated, "}",sep="")
css_generated <- ""
for (i in 1: length(colors))
{
  css_generated<-paste(css_generated, ".x", i, "{background-color: ", colors[i],";}","table.dataTable tr.selected td.x",i,"{background-color: ",colors[i], " !important;}",sep="")     
  css_generated<-paste(css_generated," ", sep="\n")
  }
print(css_generated)

# rowCallback <- c(
#   "function(row, dat, displayNum, index){",
#   "  if(dat[0]==1){",
#   "    $('td:eq(0)', row).addClass('x1');",
#   "  }",
#   "  if(dat[0]==2){",
#   "    $('td:eq(0)', row).addClass('x2');",
#   "  }",
#   "}"
# )

# css <- "
# .x1{
#   background-color: #e34755;
# }
# table.dataTable tr.selected td.x1 {
#   background-color: #e34755 !important;
# }
# 
# .x2{
#   background-color: #99d8c9
# ;
# }
# table.dataTable tr.selected td.x2 {
#   background-color: #99d8c9 !important;
# }
# "


ui <- fluidPage(
  tags$head(
    tags$style(HTML(css_generated))
  ),
  fluidRow(
    DTOutput('x1')
  )
)

server <- function(input, output) {
  output$x1 <- renderDT({
    datatable(cars,
              options = list( rownames=T,rowCallback = JS(rowCallback_generated)))})
  

}

shinyApp(ui, server)