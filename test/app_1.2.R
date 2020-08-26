library(shiny)
library(leaflet)
library(leaflet.extras)
library(ncdf4)
library(sp)
library(data.table)
library(raster)
library(rgdal)
library(dplyr)
library(lubridate)
library(RiverLoad)
library(ggplot2)
library(nycflights13)
library(readr)
######DATA
p = brick('pr.nc')

#########USER INTERFACE
ui <- fluidPage(
  titlePanel("SWAMP Kalkulaèka"),
  leafletOutput("mymap",height=800)
  #sidebarPanel(
    #selectInput(XXXXX, "Zvolte rok k zobrazení srážkového prùmìru:",
     #           choices = c("1986", "1987", "1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018")),
    #actionButton("button","Vypoèti")
  )

#####SERVER
server <- function(input, output) {
  
  output$mymap <- renderLeaflet(
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      setView(lng =  14.420609, lat =  50.086989, zoom = 10) %>%
      addDrawToolbar(
        targetGroup='draw',
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>%
      addLayersControl(overlayGroups = c('draw'), options =
                         layersControlOptions(collapsed=FALSE))
      )

#VYTAHNUTI DAT Z NETCDF   
  observeEvent(input$mymap_draw_new_feature,{
    feature <- input$mymap_draw_new_feature
    
    print(feature)
    print(class(feature))
    poly1 = melt(feature$geometry$coordinates)
    xy=dcast.data.table(data.table(poly1), L1+L2~L3, value.var = "value")
    pol = SpatialPolygons(list(Polygons(list(Polygon(xy[,.(`1`, `2`)])), "p1")))
    pol = rotatePoly(pol)
    dta = extract(p, pol, fun = mean)
    
    #VYTVORENI DATUMU  
    dta = data.table(DTM = colnames(dta), pr = dta[1,])
    dta[, DTM := as.IDate(DTM, format = 'X%Y.%m.%d')]
    View(dta)

    #PRUMER Z MESICU
      dta_month = dta %>%
      mutate(year = year(DTM), month = month(DTM)) %>%
      group_by(month) %>%
      summarise(mean_pr = mean(pr))
    plot(dta_month, type = 'l', main = "Prumerne mesicni srazky od roku 1986", xlab="mesic", ylab="srazky [mm]")
  })
}  

####SPUSTI APPKU
shinyApp(ui, server)
