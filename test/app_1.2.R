######DATA
p = brick('pr.nc')

#########USER INTERFACE
ui <- fluidPage(titlePanel("SWAMP Kalkulaèka"),
                leafletOutput("mymap", height = 800))

#####SERVER
server <- function(input, output) {
  output$mymap <- renderLeaflet(
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      setView(lng =  14.420609, lat =  50.086989, zoom = 10) %>%
      addDrawToolbar(
        targetGroup = 'draw',
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
      )  %>%
      addLayersControl(
        overlayGroups = c('draw'),
        options =
          layersControlOptions(collapsed = FALSE)
      )
  )
  
  #VYTAHNUTI DAT Z NETCDF
  observeEvent(input$mymap_draw_new_feature, {
    feature <- input$mymap_draw_new_feature
    
    print(feature)
    print(class(feature))
    poly1 = melt(feature$geometry$coordinates)
    xy = dcast.data.table(data.table(poly1), L1 + L2 ~ L3, value.var = "value")
    pol = SpatialPolygons(list(Polygons(list(Polygon(
      xy[, .(`1`, `2`)]
    )), "p1")))
    pol = rotatePoly(pol)
    dta = extract(p, pol, fun = mean)
    
    #VYTVORENI DATUMU
    dta = data.table(DTM = colnames(dta), pr = dta[1, ])
    dta[, DTM := as.IDate(DTM, format = 'X%Y.%m.%d')]
    prumer_srazek = dta %>%
      mutate(year = year(DTM), month = month(DTM)) %>%
      group_by(month) %>%
      summarise(mean_pr = mean(pr))
    spotreba = 23
    prumer = as.data.table(prumer_srazek, keep.rownames = FALSE)
    graf = (prumer_srazek[, "mean_pr"] - spotreba)
    graf2 = graf %>%
      rename("bilance" = "mean_pr")
    #merge datasety
    final = cbind(prumer, graf2)
    
    #plot finalni bilance
    
  })
}
####SPUSTI APPKU
shinyApp(ui, server)
