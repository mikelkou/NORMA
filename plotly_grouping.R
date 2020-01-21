
d <- diamonds[sample(nrow(diamonds), 1000), ]
d <- plot_ly(d, x = ~carat, y = ~price, text = ~paste("Clarity: ", clarity),
             mode = "markers", color = ~carat)
d <- layout(d, title = 'Highlighting Regions with Circles',
            shapes = list(
              list(type = 'circle',
                   xref = 'x', x0 = .2, x1 = .7,
                   yref = 'y', y0 = 20, y1 = 3000,
                   fillcolor = 'rgb(50, 20, 90)', line = list(color = 'rgb(50, 20, 90)'),
                   opacity = 0.2),
              list(type = 'circle',
                   xref = 'x', x0 = .75, x1 = 1.5,
                   yref = 'y', y0 = 2500, y1 = 7500,
                   fillcolor = 'rgb(30, 100, 120)', line = list(color = 'rgb(30, 100, 120)'),
                   opacity = 0.2),
              list(type = 'circle',
                   xref = 'x', x0 = 1.6, x1 = 2.5,
                   yref = 'y', y0 = 12500, y1 = 18500,
                   fillcolor = 'rgb(90, 200, 75)', line = list(color = 'rgb(90, 200, 75)'),
                   opacity = 0.2)))







a <- members_with_NA_groups[sample(nrow(members_with_NA_groups), nrow(members_with_NA_groups)), ]
pen <- png::readPNG("selected_groups4.png")


img_width = 600
img_height = 600
scale_factor = 1
sizexx=img_width * scale_factor
sizeyx=img_height * scale_factor
s <- seq(0, 5, by = 0.5)
p <- plot_ly(a, x = ~Xn, y = ~Yn)

p <- plot_ly()
p <- add_trace(p, x=l[,1], y=l[,2], mode="markers", type="scatter", marker = list(size = 5, color = 'rgba(0, 0, 0, 1)'))
p <- layout(p, scene = list(xaxis = list(title = "A"), yaxis = list(title = "B"), zaxis = list(title = "C")))
p
p <-layout(p, scene = list(xaxis = list(title = "A", range = c(-5,5)), yaxis = list(title = "B", range = c(-5,5))   ))
p
lay
d <- plot_ly(a, x=l[,1], y=l[,2], width = 600, height = 600,
             mode = "markers", text = names(vs), hoverinfo = "text",
             transforms = list(
               type = 'filter',
               target = members_with_NA_groups$group
             )) %>%
  layout(title = '',
         # shapes = edge_shapes,
         shapes = shapes,
          xaxis =  list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE),
          yaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE),
         
         images = list(
           source = raster2uri(as.raster(pen)),
           x=0, #(gwnia fwtografias)
           y=0,
           sizex=1.2,
           sizey=1.4,
           xref = "x", yref = "y",
            sizing = "stretch",
           opacity = 0.8,
           layer = "below",
           xanchor = "left", yanchor = "bottom"
         ),
         
         margin = list(t = 50)
  )
d
lay

l<- layout_nicely(g_virt)
l
min_x<-min(l[,1])
min_y<-min(l[,2])
for (i in 1:length(l[,1]))
{
  l[i,1]<-l[i,1]-min_x
}
l
for (i in 1:length(l[,2]))
{
  l[i,2]<-l[i,2]-min_y
}
l

d <- layout(d, title = 'Highlighting Regions with Circles',
            shapes = list(
              list(type = 'circle',
                   xref = 'x', x0 = .2, x1 = .7,
                   yref = 'y', y0 = 20, y1 = 3000,
                   fillcolor = 'rgb(50, 20, 90)', line = list(color = 'rgb(50, 20, 90)'),
                   opacity = 0.2),
              list(type = 'circle',
                   xref = 'x', x0 = .75, x1 = 1.5,
                   yref = 'y', y0 = 2500, y1 = 7500,
                   fillcolor = 'rgb(30, 100, 120)', line = list(color = 'rgb(30, 100, 120)'),
                   opacity = 0.2),
              list(type = 'circle',
                   xref = 'x', x0 = 1.6, x1 = 2.5,
                   yref = 'y', y0 = 12500, y1 = 18500,
                   fillcolor = 'rgb(90, 200, 75)', line = list(color = 'rgb(90, 200, 75)'),
                   opacity = 0.2)))
d



p <- plot_ly(
  x = mtcars$hp,
  y = mtcars$qsec,
  text = paste("Make: ", rownames(mtcars),
               "<br>hp: ", mtcars$hp,
               "<br>qsec: ", mtcars$qsec,
               "<br>Cyl: ", mtcars$cyl),
  hoverinfo = 'text',
  mode = 'markers',
  transforms = list(
    list(
      type = 'groupby',
      groups = mtcars$cyl,
      styles = list(
        list(target = 4, value = list(marker =list(color = 'blue'))),
        list(target = 6, value = list(marker =list(color = 'red'))),
        list(target = 8, value = list(marker =list(color = 'black')))
      )
    )
  )
)

p

a <- members_with_NA_groups[sample(nrow(members_with_NA_groups), nrow(members_with_NA_groups)), ]

d <- plot_ly(a, x = ~Xn, y = ~Yn, 
             mode = "markers", text = names(vs), hoverinfo = "text",
             transforms = list(
               type = 'filter',
               target = members_with_NA_groups$group
             )) %>%
  layout(d, title = '',
         shapes = list(
           list(type = 'circle',
                xref = 'x', x0 = 0.1, x1 = 0.7,
                yref = 'y', y0 = .5, y1 = .1,
                fillcolor = 'rgb(50, 20, 90)', line = list(color = 'rgb(50, 20, 90)'),
                opacity = 0.2)
         ))
  d
  layout(title = '',
         shapes = edge_shapes,
         xaxis = axis,
         yaxis = axis,
         images = list(
           source = raster2uri(as.raster(pen)),
           x = 0.5, y = 0.5,
           sizex = 0.2, sizey = 0.1,
           xref = "x", yref = "y",
           # sizing = "stretch",
           opacity = 0.8,
           layer = "below",
           xanchor = "left", yanchor = "bottom"
         ),

         margin = list(t = 50)
  )
m <- layout(d, title = 'Highlighting Regions with Circles',
            shapes = list(
              list(type = 'circle',
                   xref = 'x', x0 = 0.1, x1 = 0.2,
                   yref = 'y', y0 = .05, y1 = .1,
                   fillcolor = 'rgb(50, 20, 90)', line = list(color = 'rgb(50, 20, 90)'),
                   opacity = 0.2)
              ))
m
