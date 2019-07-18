library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(leaflet)
library(rgdal)
library(sf)
library(lubridate)
library(grDevices)
library(plotly)
library(data.table)
library(raster)
library(scales)
####### THIS WEEK ########
### Cursor as bar on graph  to show all values at a given time
### Fix point emissions and roads palettes & Home page color palette
### Downloads page --> fix shapefile download
### About Page --> Add content 


##### DATA LOADING START #####
source("DashFunctions.R")

master.raster <- stack("Data/Master_Raster.tif")
raster.names <- read.csv("Data/Master_Raster_Names.csv")
names(master.raster) <- raster.names$names.master.grid.

large.area <- st_read("Data/LargeAreaCounties")
large.area$COUNTYNAME <- as.character(large.area$COUNTYNAME)

descriptions <- read.csv("Data/Description.csv", stringsAsFactors = F)

county.avgs <- read.csv("Data/county_averages_monthly.csv")
county.avgs$Name <- as.character(county.avgs$Name)

var.avgs <- colMeans(county.avgs[,4:ncol(county.avgs)], na.rm = T)

 ##### DATA LOADING END #####

##### VARIABLE START #####

mapheight = 500

##### AOD START #####
aod.tabname <- "aod"
aod.name <- "Aerosol Optical Depth"

aod.description <- descriptions$Description[descriptions["Variable"] == "AOD"]
aod.source <- descriptions$Source[descriptions["Variable"] == "AOD"]

##### AOD END #####

##### NDVI START #####
ndvi.tabname <- "ndvi"
ndvi.name <- "Normalized Difference Vegetation Index"
ndvi.description <- descriptions$Description[descriptions["Variable"] == "NDVI"]
ndvi.source <- descriptions$Source[descriptions["Variable"] == "NDVI"]

##### NDVI END #####

##### BRF START #####

brf.tabname <- "brf"
brf.name <- "Bidirectional Reflectance Factor"
brf.description <- descriptions$Description[descriptions["Variable"] == "BRF"]
brf.source <- descriptions$Source[descriptions["Variable"] == "BRF"]

##### BRF END #####

##### LAND COVER START #####
lc.tabname <- "landcover"
lc.name <- "Land Cover"
lc.description <- descriptions$Description[descriptions["Variable"] == "Land Cover"]
lc.source <- descriptions$Source[descriptions["Variable"] == "Land Cover"]

##### ELEVATION START #####

elevation.tabname <- "elevation"
elevation.name <- "Elevation"
elevation.description <- descriptions$Description[descriptions["Variable"] == "Elevation"]
elevation.source <- descriptions$Source[descriptions["Variable"] == "Elevation"]


##### ELEVATION END #####

##### PM2.5 START #####

pm25.tabname <- "pm25"
pm25.name <- "Particulate Matter < 2.5μm (PM2.5)"
pm25.description <- descriptions$Description[descriptions["Variable"] == "PM2.5"]
pm25.source <- descriptions$Source[descriptions["Variable"] == "PM2.5"]

##### PM2.5 END #####

##### PM10 START #####

pm10.tabname <- "pm10"
pm10.name <- "Particulate Matter < 10μm (PM10)"
pm10.description <- descriptions$Description[descriptions["Variable"] == "PM10"]
pm10.source <- descriptions$Source[descriptions["Variable"] == "PM10"]


##### PM10 END #####

##### CO START #####
co.tabname <- "co"
co.name <- "Carbon Monoxide"
co.description <- descriptions$Description[descriptions["Variable"] == "CO"]
co.source <- descriptions$Source[descriptions["Variable"] == "CO"]
##### CO END #####

##### NO2 START #####

nox.tabname <- "nox"
nox.name <- "Nitrogen Dioxide"
nox.description <- descriptions$Description[descriptions["Variable"] == "NO2"]
nox.source <-  descriptions$Source[descriptions["Variable"] == "NO2"]

##### NO2 END #####

##### O3 START #####

o3.tabname <- "o3"
o3.name <- "Ozone"
o3.description <- descriptions$Description[descriptions["Variable"] == "Ozone"]
o3.source <- descriptions$Source[descriptions["Variable"] == "Ozone"]

##### O3 END #####

##### SO2 START #####

so2.tabname <- "so2"
so2.name <- "Sulfur Dioxide"
so2.description <- descriptions$Description[descriptions["Variable"] == "SO2"]
so2.source <- descriptions$Source[descriptions["Variable"] == "SO2"]

##### SO2 END #####

##### PB START #####

pb.tabname <- "pb"
pb.name <- "Lead"
pb.description <- descriptions$Description[descriptions["Variable"] == "Pb"]
pb.source <- descriptions$Source[descriptions["Variable"] == "Pb"]

##### PB END #####

##### PE START #####

pe.tabname <- "pe"
pe.name <- "Point Emissions"
pe.description <- descriptions$Description[descriptions["Variable"] == "Point Emissions"]
pe.source <- descriptions$Source[descriptions["Variable"] == "Point Emissions"]

##### PE END #####

##### ROADS START #####

roads.tabname <- "roads"
roads.name <- "Road Emissions"
roads.description <- descriptions$Description[descriptions["Variable"] == "Roads"]
roads.source <- descriptions$Source[descriptions["Variable"] == "Roads"]

##### ROADS END #####

##### TEMP START #####

temp.tabname <- "temp"
temp.name <- "Temperature"
temp.description <- descriptions$Description[descriptions["Variable"] == "Temperature"]
temp.source <- descriptions$Source[descriptions["Variable"] == "Temperature"]

##### TEMP END #####

##### COLOR PALETTE START #####

pe.breaks <- seq(from = 0, to = 8, 1) #Figure out way to incorporate the 1 place w 15 emissions sources
pe.pal <- colorNumeric(c("green", "yellow", "orange", "darkorange",
                         "red", "red4", "purple", "purple4"), pe.breaks, na.color = "transparent")

roads.breaks <- seq(from = 0, to = 0.03, 0.005)
roads.pal <- colorNumeric(c("green", "yellow", "orange", "red"), roads.breaks, na.color = "transparent")

##### COLOR PALETTE END #####

##### VARIABLE END #####

##### THEME START #####

chicago_blue <- "rgb(128, 206, 255)"
chicago_red <- "rgb(199, 20, 20)"

sidebar_select_gradient <- cssGradientThreeColors(
  direction = "right"
  ,colorStart = "rgb(255, 67, 67)"
  ,colorMiddle = "rgb(255, 120, 120)"
  ,colorEnd = "rgb(255,175,175)"
  ,colorStartPos = 0
  ,colorMiddlePos = 30
  ,colorEndPos = 100
)

# sidebar_hover_gradient <- cssGradientThreeColors(
#   direction = "right"
#   ,colorStart = chicago_red
#   ,colorMiddle = "rgba(199,80,80,1)"
#   ,colorEnd = "rgba(199,110,110, 1)"
#   ,colorStartPos = 0
#   ,colorMiddlePos = 30
#   ,colorEndPos = 100
# )
sidebar_hover_gradient <- sidebar_select_gradient

### creating custom theme object
theme_air_chicago <- shinyDashboardThemeDIY(

  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(217,217,217)"

  ### header
  ,logoBackColor = chicago_blue

  ,headerButtonBackColor = chicago_blue
  ,headerButtonIconColor = "rgb(245,245,245)"
  ,headerButtonBackColorHover = chicago_blue
  ,headerButtonIconColorHover = "rgb(0,0,0)"

  ,headerBackColor = chicago_blue
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"

  ### sidebar
  ,sidebarBackColor = chicago_blue
  ,sidebarPadding = 0

  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0

  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"

  ,sidebarUserTextColor = "rgb(255,255,255)"

  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"

  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 1

  ,sidebarTabBackColorSelected = sidebar_select_gradient
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"

  ,sidebarTabBackColorHover = sidebar_hover_gradient
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"

  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"

  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5

  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5

  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"

  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"

  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
)

##### THEME END #####

ui <- dashboardPage(

  ##### LOGO START #####
  dashboardHeader(title = shinyDashboardLogoDIY(boldText = "Open Air",
                                                    mainText = "Chicago",
                                                    textSize = 16,
                                                    badgeText = "BETA",
                                                    badgeTextColor = "white",
                                                    badgeTextSize = 2,
                                                    badgeBackColor = chicago_red,
                                                    badgeBorderRadius = 3)
                      ),
  ##### LOGO END #####

  dashboardSidebar(sidebarMenu(id = "sidebar",
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("About", tabName = "about", icon = icon("info")),
    menuItem("Remote-Sensed Data", icon = icon("wifi"),
             menuSubItem("Aerosol Optical Depth", tabName = "aod"),
             menuSubItem("NDVI", tabName = "ndvi"),
             menuSubItem("BRF", tabName = "brf"),
             menuSubItem("Land Cover", tabName = "landcover"),
             menuSubItem("Elevation", tabName = "elevation")),
    menuItem("EPA Sensor Data", icon = icon("envira"),
             menuSubItem("PM2.5", tabName = "pm25"),
             menuSubItem("PM10", tabName = "pm10"),
             menuSubItem("Carbon Monoxide", tabName = "co"),
             menuSubItem("Nitrogen Dioxide", tabName = "nox"),
             menuSubItem("Ozone", tabName = "o3"),
             menuSubItem("Sulfur Dioxide", tabName = "so2"),
             menuSubItem("Lead", tabName = "pb")),
    menuItem("Human Emissions", icon = icon("industry"),
             menuSubItem("Point Emissions", tabName = "pe"),
             menuSubItem("Roads", tabName = "roads")),
    menuItem("Meteorological Data", icon = icon("thermometer-half"),
             menuSubItem("Temperature", tabName = "temp")),
             #menuSubItem("Precipitation", tabName = "precip")),
    menuItem("Downloads", icon = icon("download"), tabName = "downloads"))
  ),

  dashboardBody(

    theme_air_chicago,

    tabItems(

    ##### HOME START #####
    tabItem(tabName = "home",
      fluidRow(
        box(width = 12,
            h1("Home", align = "center")
      )),
      fluidRow(
        box(width = 4,
            leafletOutput("homemap", height = mapheight),
            checkboxGroupInput("homecheck", label = "", c("Show Mean" = "mean",
                                                          "Rescale Data" = "rescale"),
                               selected = c("mean"),
                               width = '100%',
                               inline = TRUE)),
        box(width = 8,
            selectizeInput("homevar", "Select Variables for Comparison:",
                           c("AOD" = "AOD",
                             "NDVI" = "NDVI",
                             "BRF" = "BRF",
                             "PM2.5" = "PM25",
                             "PM10" = "PM10",
                             "Carbon Monoxide" = "CO",
                             "Nitrogen Dioxide" = "NO2",
                             "Ozone" = "Ozone",
                             "Sulfur Dioxide" = "SO2",
                             "Lead" = "Lead",
                             "Temperature" = "Temp"),
                           options = list(maxItems = 7)),
            plotlyOutput("homeplot", height = 425)))
    ),
    ##### HOME END #####
    ##### ABOUT START #####
    tabItem(tabName = "about",
            fluidRow(
              box(width = 12,
                  h1("About", align = "center"),
                  textOutput("abouttext")
              )),
            fluidRow(
              
              h1("INSERT TEXT")
            )
    ),
    ##### ABOUT END #####

    generateQuarterlyTab(aod.tabname, aod.name, aod.description, aod.source),

    generateQuarterlyTab(ndvi.tabname, ndvi.name, ndvi.description, ndvi.source),

    generateQuarterlyTab(brf.tabname, brf.name, brf.description, brf.source),

    ##### LAND USE START #####
    tabItem(tabName = "landcover",
            fluidRow(
              box(width = 4,
                  tabsetPanel(
                    tabPanel(title = "Description",
                             h3(lc.name),
                             p(lc.description)),
                    tabPanel(title = "Source",
                             h4("Data Source"),
                             p(lc.source)))),
              box(width = 8,
                  tabsetPanel(
                    tabPanel("Green Index", leafletOutput("grn_map", height = mapheight)),
                    tabPanel("Gray Index", leafletOutput("gry_map", height = mapheight)),
                    tabPanel("Blue Index", leafletOutput("blu_map", height = mapheight)))
                  )
            )
    ),
    ##### LAND USE END #####

    generateOneTimeTab(elevation.tabname, elevation.name, elevation.description, elevation.source),

    generateQuarterlyTab(pm25.tabname, pm25.name, pm25.description, pm25.source),

    generateQuarterlyTab(pm10.tabname, pm10.name, pm10.description, pm10.source),

    generateQuarterlyTab(co.tabname, co.name, co.description, co.source),

    generateQuarterlyTab(nox.tabname, nox.name, nox.description, nox.source),

    generateQuarterlyTab(o3.tabname, o3.name, o3.description, o3.source),

    generateQuarterlyTab(so2.tabname, so2.name, so2.description, so2.source),

    generateQuarterlyTab(pb.tabname, pb.name, pb.description, pb.source),

    generateOneTimeTab(pe.tabname, pe.name, pe.description, pe.source),

    generateOneTimeTab(roads.tabname, roads.name, roads.description, roads.source),

    generateQuarterlyTab(temp.tabname, temp.name, temp.source, temp.description),

    ##### PRECIPITATION START #####
    tabItem(tabName = "precip",
            fluidRow(
              box(
                width = 4,
                h3("Precipitation"),
                p("Aerosol optical depth is a measure of the extinction of the solar beam by dust
                  and haze. In other words, particles in the atmosphere (dust, smoke, pollution)
                  can block sunlight by absorbing or by scattering light."), #### NEATER DEFINITION
                br(),
                h4("Data Source"),
                p("We use data directly from NASA. The Moderate Resolution Imaging Spectroradiometer
                  (MODIS) satellite provides daily global coverage, but the 10 km resolution of its
                  aerosol optical depth (AOD) product is not suitable for studying spatial variability
                  of aerosols in urban areas. Recently, a new Multi-Angle Implementation of Atmospheric
                  Correction (MAIAC) algorithm was developed for MODIS which provides AOD at 1 km
                  resolution.") #### FIX

                ),

              box(width = 8,
                  sliderInput("precip_dt", "Select quarter:",
                              min = strptime("2014/01/01","%Y/%m/%d"),
                              max = strptime("2018/12/31","%Y/%m/%d"),
                              value = strptime("2016/07/01","%Y/%m/%d"),
                              timeFormat = "%Y/%m",
                              step = as.difftime(92, units = "days"),
                              animate = animationOptions(interval = 2000)),
                  leafletOutput("precip_map",height = mapheight))

                )),
    ##### PRECIPITATION END #####
    
    
    ##### DOWNLOADS START #####
    
    tabItem(tabName = "downloads",
            fluidRow(
              box(width = 12,
                  h1("Downloads", align = "center")
            )),
            
            fluidRow(
              box(width = 4,
                  h3("CSV", align = "center"),
                  downloadBttn("monthly_data",
                               label = "Download Monthly County Data",
                               style = "simple"),
                  downloadBttn("quarterly_data",
                               label = "Download Quarterly County Data",
                               style = "simple")),
              box(width = 4,
                  h3("Raster", align = "center"),
                  downloadBttn("master_raster",
                               label = "Download 1km Resolution Raster",
                               style = "simple"),
                  downloadBttn("master_raster_names",
                               label = "Download Raster Layer Names", 
                               style = "simple")),
              box(width = 4,
                  h3("Shapefile", align = "center"),
                  downloadBttn("large_area_counties",
                               label = "Download County Shapefile",
                               style = "simple"))
            ))
    
    
    ##### DOWNLOADS END #####

)))

server <- function(input, output) {


  ##### HOME START #####
  
  abt.count <- reactiveValues(val = 0) #Create counter to track hold of last shape clicked
  all.fips <- reactiveValues(fips = c())
  
  
  output$homemap <- renderLeaflet({
    leaflet(large.area) %>%
      addProviderTiles(provider = "Hydda.Full") %>%
      leaflet::addPolygons(weight = 1,
                           color = "gray",
                           layerId = large.area$FIPS,
                           fillOpacity = 0.2,
                           label = large.area$COUNTYNAME,
                           highlight = highlightOptions(
                             weight = 2,
                             color = "#666",
                             fillOpacity = 0.7,
                             bringToFront = TRUE))
  })
  
  # Highlight clicked counties, unhighlight double clicked, zoom to center of all selected
  observeEvent(input$homemap_shape_click, {
    if(input$sidebar == "home") { #Optimize Dashboard speed by not observing outside of tab
      
      this.fips <- input$homemap_shape_click$id
      
      home.proxy <- leafletProxy("homemap")
      
      if(nchar(this.fips) <= 5) { #Make sure that selected layer not highlighted
        abt.count$val <- abt.count$val + 1
        all.fips$fips[abt.count$val] <- this.fips
        
        for(i in 1:length(all.fips$fips)) { #Highlight selected counties
          home.proxy <- home.proxy %>%
            addPolygons(data = large.area[which(large.area$FIPS %in% all.fips$fips[i]),][1],
                        color = "red", layerId = paste("Highlighted", all.fips$fips[i]),
                        label = paste(large.area$COUNTYNAME[which(large.area$FIPS %in% all.fips$fips[i])], " County"))
        }
        
      } else {
        high.fips <- substring(this.fips, first = 13) #Extract FIPS from Layer Id
        home.proxy <- home.proxy %>%
          removeShape(layerId = c(paste("Highlighted", all.fips$fips)))  #Clear highlighted shapes from proxy
        
        all.fips$fips <- all.fips$fips[-which(all.fips$fips %in% high.fips)] #Remove fips from list of selected fips
        
        abt.count$val <- abt.count$val - 1 #Adjust for removed value
        
        for(i in 1:length(all.fips$fips)) { #Highlight all remaining counties 
          home.proxy <- home.proxy %>%
            addPolygons(data = large.area[which(large.area$FIPS %in% all.fips$fips[i]),][1],
                        color = "red", layerId = paste("Highlighted", all.fips$fips[i]),
                        label = paste(large.area$COUNTYNAME[which(large.area$FIPS %in% all.fips$fips[i])], " County"))
        }
      }
      
      view.lat <- mean(large.area$LAT[which(large.area$FIPS %in% all.fips$fips)])
      view.lon <- mean(large.area$LON[which(large.area$FIPS %in% all.fips$fips)])
      
      home.proxy %>%
        setView(lng = view.lon,
                lat = view.lat, 
                zoom = 7)
      
    }
  })
  
  output$homeplot <- renderPlotly({
    
    ##### Transform averages into plotly friendly format
    vars <- input$homevar
 
    if(is.null(vars)){
      p <- plot_ly() %>% config(displayModeBar = F) %>%
        layout(legend = list(x = .5, y = 100, orientation = "h"))
      p
      return()
    }

    these.vars.avgs <- list()
    
    for(i in 1:length(vars)) {
      these.vars.avgs[[i]] <- var.avgs[which(grepl(vars[i], names(var.avgs)))]
      
      if ("rescale" %in% input$homecheck) {
        these.vars.avgs[[i]] <- rescale(these.vars.avgs[[i]])
      }
    }
    

    
    months <- 1:12
    years <- 2014:2018
    dates <- c()
    
    for(i in 1:length(years)) {
      this.yr <- paste(months, "01", years[i], sep = "-")
      dates <- c(dates, this.yr)
      
    }
    dates <- mdy(as.character(dates))
    
    highlighted <- all.fips$fips
    
    if(length(all.fips$fips == 0)) {
      selected.fips <- 0
    }
    
    selected.fips <- which(county.avgs$FIPS %in% all.fips$fips)

    colors <- c("#0026ff", "#ff0000", "#000000",
                "#af03ff", "#ff00e1")
    
    home.checkbox <- ("mean" %in% input$homecheck)
    
    p <- plot_ly() %>% config(displayModeBar = F) %>%
      layout(legend = list(x = .5, y = 100, orientation = "h"))
    
    for(i in 1:length(these.vars.avgs)) {
      if(home.checkbox == T) { # Add overall variable mean line
        p <- add_trace(p,
                       x = dates,
                       y = these.vars.avgs[[i]],
                       type = "scatter",
                       mode = "lines",
                       opacity = 1,
                       line = list(dash = "dot", color = colors[i]),
                       name = paste("Average", vars[i], sep = " "),
                       text= paste("Average", vars[i], sep = " "))
      }
      if(length(selected.fips) != 0) { # Add county variable mean line
        for(j in 1:length(selected.fips)) {
          if("rescale" %in% input$homecheck) {
          p <- add_trace(p,
                         x = dates,
                         y = rescale(as.numeric(county.avgs[selected.fips[j],names(these.vars.avgs[[i]])])),
                         type = "scatter",
                         mode = "lines",
                         opacity = .5,
                         #line = list(color = colors[j]),
                         name = paste(county.avgs$Name[selected.fips[j]], "County", vars[i], sep = " "),
                         text= paste(county.avgs$Name[selected.fips[j]], "County", vars[i], sep = " "))
          } else {
            p <- add_trace(p,
                           x = dates,
                           y = as.numeric(county.avgs[selected.fips[j],names(these.vars.avgs[[i]])]),
                           type = "scatter",
                           mode = "lines",
                           opacity = .5,
                           #line = list(color = colors[j]),
                           name = paste(county.avgs$Name[selected.fips[j]], "County", vars[i], sep = " "),
                           text= paste(county.avgs$Name[selected.fips[j]], "County", vars[i], sep = " "))
          }
        }}
    }
    p
  })
  

  ##### HOME END #####

  ##### ABOUT START #####
  

  ##### ABOUT END #####


  output$aod_map <- renderLeaflet({

      this.aod.name <- "AOD_3_16"

      in.pal <- "ovr"

      aod.pal <- palFromLayer(this.aod.name, style = in.pal, raster = master.raster)

      dashMap(this.aod.name, aod.pal, raster = master.raster, area = large.area,
              layerId = large.area$FIPS)

  })

  observe({
    if (input$sidebar == "aod") { #Optimize Dashboard speed by not observing outside of tab
    in.date <- input$aod_dt
    this.aod.name <- getLayerName(in.date, "AOD")

    in.pal <- input$aod_rad

    aod.pal <- palFromLayer(this.aod.name, style = in.pal, raster = master.raster)

    sliderProxy("aod_map", this.aod.name, aod.pal, raster = master.raster)
    }
  })
  
  observeEvent(input$aod_map_shape_click, {
    if(input$sidebar == "aod") { #Optimize Dashboard speed by not observing outside of tab
      click <- input$aod_map_shape_click
      
      zoomMap("aod_map", click, large.area)
      
    }
  })

  output$ndvi_map <- renderLeaflet({
    this.ndvi.name <- "NDVI_3_16"
    in.pal <- "ovr"

    ndvi.pal <- palFromLayer(this.ndvi.name, style = in.pal, colors = c("lightblue", "yellow", "lightgreen", "green", "darkgreen"),
                             raster = master.raster)

    dashMap(this.ndvi.name, ndvi.pal, raster = master.raster, area = large.area, 
            layerId = large.area$FIPS)

  })

  observe({
    if (input$sidebar == "ndvi") {
    in.date <- input$ndvi_dt
    this.ndvi.name <- getLayerName(in.date, "NDVI")

    in.pal <- input$ndvi_rad

    ndvi.pal <- palFromLayer(this.ndvi.name, style = in.pal, colors = c("lightblue", "yellow", "lightgreen", "green", "darkgreen"),
                             raster = master.raster)

    sliderProxy("ndvi_map", this.ndvi.name, ndvi.pal, raster = master.raster)
    }
  })
  
  observeEvent(input$ndvi_map_shape_click, {
    if(input$sidebar == "ndvi") { 
      click <- input$ndvi_map_shape_click
      
      zoomMap("ndvi_map", click, large.area)
    }
  })

  output$brf_map <- renderLeaflet({

    this.brf.name <- "BRDF_3_16"

    in.pal <- "ovr"

    brf.pal <- palFromLayer(this.brf.name, style = in.pal, colors = c("black", "white"), raster = master.raster)

    brf.map <- dashMap(this.brf.name, brf.pal, raster = master.raster, area = large.area, layerId = large.area$FIPS)

  })

  observe({
    if (input$sidebar == "brf") {
      in.date <- input$brf_dt
      this.brf.name <- getLayerName(in.date, "BRDF")
      
      in.pal <- input$brf_rad
      
      brf.pal <- palFromLayer(this.brf.name, style = in.pal, colors = c("black", "white"), raster = master.raster)
      
      sliderProxy("brf_map", this.brf.name, brf.pal, raster = master.raster)
    }
  })
  
  observeEvent(input$brf_map_shape_click, {
    if(input$sidebar == "brf") { 
      click <- input$brf_map_shape_click
      
      zoomMap("brf_map", click, large.area)
    }
  })

  output$grn_map <- renderLeaflet({
    grn.pal <- palFromLayer("grn_ndx", colors = c("white","lightgreen", "green", "darkgreen"), raster = master.raster)
    grn.map <- dashMap("grn_ndx", grn.pal, raster = master.raster, area = large.area,
                       layerId = large.area$FIPS)
  })

  output$gry_map <- renderLeaflet({
    gry.pal <- palFromLayer("gry_ndx", colors = c("white", "lightgray", "gray", "darkgray", "black"), raster = master.raster)
    gry.map <- dashMap("gry_ndx", gry.pal, raster = master.raster, area = large.area, layerId = large.area$FIPS)
  })

  output$blu_map <- renderLeaflet({
    blu.pal <- palFromLayer("blu_ndx", colors = c("white","lightblue", "blue", "darkblue"), raster = master.raster)
    blu.map <- dashMap("blu_ndx", blu.pal, raster = master.raster, area = large.area, layerId = large.area$FIPS)
  })

  output$elevation_map <- renderLeaflet({

    elev.pal <- palFromLayer("Elev", raster = master.raster)

    elevation.map <- dashMap("Elev", elev.pal, raster = master.raster, area = large.area, layerId = large.area$FIPS)

  })

  output$pm25_map <- renderLeaflet({

    this.pm25.name <- "PM25_3_16"

    in.pal <- "ovr"

    pm25.pal <- palFromLayer(this.pm25.name, style = in.pal, raster = master.raster)

    dashMap(this.pm25.name, pm25.pal, raster = master.raster, area = large.area, layerId = large.area$FIPS)

  })

  observe({
    if (input$sidebar == "pm25") {
    in.date <- input$pm25_dt
    this.pm25.name <- getLayerName(in.date, "PM25")

    in.pal <- input$pm25_rad

    pm25.pal <- palFromLayer(this.pm25.name, style = in.pal, raster = master.raster)

    sliderProxy("pm25_map", this.pm25.name, pm25.pal, raster = master.raster)
    }
  })
  
  observeEvent(input$pm25_map_shape_click, {
    if(input$sidebar == "pm25") { 
      click <- input$pm25_map_shape_click
      
      zoomMap("pm25_map", click, large.area)
    }
  })

  output$pm10_map <- renderLeaflet({

    this.pm10.name <- "PM10_3_16"

    in.pal <- "ovr"

    pm10.pal <- palFromLayer(this.pm10.name, style = in.pal, raster = master.raster)

    dashMap(this.pm10.name, pm10.pal, raster = master.raster, area = large.area, layerId = large.area$FIPS)

  })

  observe({
    if (input$sidebar == "pm10") {
    in.date <- input$pm10_dt
    this.pm10.name <- getLayerName(in.date, "PM10")

    in.pal <- input$pm10_rad

    pm10.pal <- palFromLayer(this.pm10.name, style = in.pal, raster = master.raster)

    sliderProxy("pm10_map", this.pm10.name, pm10.pal, raster = master.raster)
    }
  })
  
  observeEvent(input$pm10_map_shape_click, {
    if(input$sidebar == "pm10") { 
      click <- input$pm10_map_shape_click
      
      zoomMap("pm10_map", click, large.area)
    }
  })

  output$co_map <- renderLeaflet({

    this.co.name <- "CO_3_16"

    in.pal <- "ovr"

    co.pal <- palFromLayer(this.co.name, style = in.pal, raster = master.raster)

    dashMap(this.co.name, co.pal, raster = master.raster, area = large.area, layerId = large.area$FIPS)

  })

  observe({
    if (input$sidebar == "co") {
      in.date <- input$co_dt
      this.co.name <- getLayerName(in.date, "CO")

      in.pal <- input$co_rad

      co.pal <- palFromLayer(this.co.name, style = in.pal, raster = master.raster)

      sliderProxy("co_map", this.co.name, co.pal, raster = master.raster)
    }
  })
  
  observeEvent(input$co_map_shape_click, {
    if(input$sidebar == "co") { 
      click <- input$co_map_shape_click
      
      zoomMap("co_map", click, large.area)
    }
  })

  output$nox_map <- renderLeaflet({

    this.nox.name <- "NO2_3_16"

    in.pal <- "ovr"

    nox.pal <- palFromLayer(this.nox.name, style = in.pal, raster = master.raster)

    dashMap(this.nox.name, nox.pal, raster = master.raster, area = large.area, layerId = large.area$FIPS)

  })

  observe({
    if(input$sidebar == "nox") {
    in.date <- input$nox_dt
    this.nox.name <- getLayerName(in.date, "NO2")

    in.pal <- input$nox_rad

    nox.pal <- palFromLayer(this.nox.name, style = in.pal, raster = master.raster)

    sliderProxy("nox_map", this.nox.name, nox.pal, raster = master.raster)
    }
  })
  
  observeEvent(input$nox_map_shape_click, {
    if(input$sidebar == "nox") { 
      click <- input$nox_map_shape_click
      
      zoomMap("nox_map", click, large.area)
    }
  })


  output$o3_map <- renderLeaflet({

    this.o3.name <- "Ozone_3_16"
    in.pal <- "ovr"

    o3.pal <- palFromLayer(this.o3.name, style = in.pal, raster = master.raster)

    dashMap(this.o3.name, o3.pal, raster = master.raster, area = large.area, layerId = large.area$FIPS)

  })

  observe({
    if (input$sidebar == "o3") {
    in.date <- input$o3_dt
    this.o3.name <- getLayerName(in.date, "Ozone")

    in.pal <- input$o3_rad

    o3.pal <- palFromLayer(this.o3.name, style = in.pal, raster = master.raster)

    sliderProxy("o3_map", this.o3.name, o3.pal, raster = master.raster)
    }
  })
  
  observeEvent(input$o3_map_shape_click, {
    if(input$sidebar == "o3") { 
      click <- input$o3_map_shape_click
      
      zoomMap("o3_map", click, large.area)
    }
  })

  output$so2_map <- renderLeaflet({

    this.so2.name <- "SO2_3_16"

    in.pal <- "ovr"

    so2.pal <- palFromLayer(this.so2.name, style = in.pal, raster = master.raster)

    dashMap(this.so2.name, so2.pal, raster = master.raster, area = large.area, layerId = large.area$FIPS)

  })

  observe({
    if(input$sidebar == "so2") {
      in.date <- input$so2_dt
      this.so2.name <- getLayerName(in.date, "SO2")

      in.pal <- input$so2_rad

      so2.pal <- palFromLayer(this.so2.name, style = in.pal, raster = master.raster)

      sliderProxy("so2_map", this.so2.name, so2.pal, raster = master.raster)
    }

  })

  observeEvent(input$so2_map_shape_click, {
    if(input$sidebar == "so2") { 
      click <- input$so2_map_shape_click
      
      zoomMap("so2_map", click, large.area)
    }
  })
  
  output$pb_map <- renderLeaflet({

    this.pb.name <- "Lead_3_16"

    in.pal <- "ovr"

    pb.pal <- palFromLayer(this.pb.name, style = in.pal, raster = master.raster)

    dashMap(this.pb.name, pb.pal, raster = master.raster, area = large.area, layerId = large.area$FIPS)

  })

  observe({
    if(input$sidebar == "pb") {

      in.date <- input$pb_dt
      this.pb.name <- getLayerName(in.date, "Lead")

      in.pal <- input$pb_rad

      pb.pal <- palFromLayer(this.pb.name, style = in.pal, raster = master.raster)

      sliderProxy("pb_map", this.pb.name, pb.pal, raster = master.raster)

    }
  })
  
  observeEvent(input$pb_map_shape_click, {
    if(input$sidebar == "pb") { 
      click <- input$pb_map_shape_click
      
      zoomMap("pb_map", click, large.area)
    }
  })

  output$pe_map <- renderLeaflet({

    pe.pal <- palFromLayer("PECount", raster = master.raster)
    pe.map <- dashMap("PECount", pe.pal, raster = master.raster, area = large.area, layerId = large.area$FIPS)

  })

  output$roads_map <- renderLeaflet({
    roads.map <- dashMap("RdDnsty", roads.pal, raster = master.raster, area = large.area, layerId = large.area$FIPS)
  })

  output$temp_map <- renderLeaflet({
    this.temp.name <- "Temp_3_16"

    in.pal <- "ovr"

    temp.pal <- palFromLayer(this.temp.name, style = in.pal, raster = master.raster)

    dashMap(this.temp.name, temp.pal, raster = master.raster, area = large.area, layerId = large.area$FIPS)
  })

  observe({
    if(input$sidebar == "temp") {
      in.date <- input$temp_dt
      this.temp.name <- getLayerName(in.date, "Temp")

      in.pal <- input$temp_rad

      temp.pal <- palFromLayer(this.temp.name, style = in.pal, raster = master.raster)

      sliderProxy("temp_map", this.temp.name, temp.pal, raster = master.raster)
    }
  })
  
  observeEvent(input$temp_map_shape_click, {
    if(input$sidebar == "temp") { 
      click <- input$temp_map_shape_click
      
      zoomMap("temp_map", click, large.area)
    }
  })

  ##### PRECIPITATION START #####

  ###### ADD IN PRECIP DATA WHEN IT"S AVAILABLE
  # output$precip_map <- renderLeaflet({
  #
  #   in.date <- input$precip_dt
  #   this.precip.name <- getLayerName(in.date, "Precip")
  #
  #   precip.pal <- palFromLayer(this.precip.name, raster = master.raster)
  #
  #   precip.map <- dashMap(this.precip.name, precip.pal, raster = master.raster, area = large.area, layerId = large.area$FIPS)
  # })
  #

  ##### PRECIPITATION END #####
  
  ##### DOWNLOADS START #####
  
  output$monthly_data <- downloadHandler(filename = "county_averages_monthly.csv", 
                                         content = function(file){
                                           write.csv(county.avgs, file)})
  
  output$quarterly_data <- downloadHandler(filename = "county_averages_quarterly.csv", 
                                           content = function(file) {
                                             file.copy("Data/county_averages_quarterly.csv", file)
                                           })
  
  output$master_raster <- downloadHandler(filename = "Master_Raster.tif",
                                          content = function(file){
                                          file.copy("Data/Master_Raster.tif", file)})
  
  output$master_raster_names <- downloadHandler(filename = "Master_Raster_Names.csv",
                                                content = function(file) {
                                                  file.copy("Data/Master_Raster_Names.csv", file)
                                                })
  
  #output$large_area_counties <- downloadHandler()

  ##### DOWNLOADS END #####
  
  

}

shinyApp(ui, server)

