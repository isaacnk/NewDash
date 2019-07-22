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

####### TO DO ########
### Find reliable precip data --> include
### Barometric pressure --> added to home, add to Raster and create page for it

##### DATA LOADING START #####
source("DashFunctions.R")

master.raster <- stack("Data/Master_Raster.tif")
raster.names <- read.csv("Data/Master_Raster_Names.csv")

names(master.raster) <- raster.names$x
large.area <- st_read("Data/LargeAreaCounties")
large.area$COUNTYNAME <- as.character(large.area$COUNTYNAME)

descriptions <- read.csv("Data/Description.csv", stringsAsFactors = F)

county.avgs <- read.csv("Data/county_averages_monthly.csv")
county.avgs$Name <- as.character(county.avgs$Name)

var.avgs <- colMeans(county.avgs[,4:ncol(county.avgs)], na.rm = T)

epa.points <- st_read("Data/EPA_Points")
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

no2.tabname <- "no2"
no2.name <- "Nitrogen Dioxide"
no2.description <- descriptions$Description[descriptions["Variable"] == "NO2"]
no2.source <-  descriptions$Source[descriptions["Variable"] == "NO2"]

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

##### PRESSURE START #####

pressure.tabname <- "pressure"
pressure.name <- "Barometric Pressure"
pressure.description <- descriptions$Description[descriptions["Variable"] == "Pressure"]
pressure.source <- descriptions$Source[descriptions["Variable"] == "Pressure"]

##### PLOT ADJUSTMENT START #####

master.raster$PECount[which(getValues(master.raster$PECount) == 0)] <- NA ### Needed for plotting; raster error when try to write new file
master.raster$RdDnsty[which(getValues(master.raster$RdDnsty) == 0)] <- NA

##### PLOT ADJUSTMENT END #####

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
  ,sidebarPadding = 2
  
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
             menuSubItem("Nitrogen Dioxide", tabName = "no2"),
             menuSubItem("Ozone", tabName = "o3"),
             menuSubItem("Sulfur Dioxide", tabName = "so2"),
             menuSubItem("Lead", tabName = "pb")),
    menuItem("Human Emissions", icon = icon("industry"),
             menuSubItem("Point Emissions", tabName = "pe"),
             menuSubItem("Roads", tabName = "roads")),
    menuItem("Meteorological Data", icon = icon("thermometer-half"),
             menuSubItem("Temperature", tabName = "temp"),
             menuSubItem("Pressure", tabName = "pressure")),
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
                           c("Aerosol Optical Depth" = "AOD",
                             "Normalized Difference Vegetation Index" = "NDVI",
                             "Bidirectional Reflectance Factor" = "BRF",
                             "PM2.5" = "PM25",
                             "PM10" = "PM10",
                             "Carbon Monoxide" = "CO",
                             "Nitrogen Dioxide" = "NO2",
                             "Ozone" = "Ozone",
                             "Sulfur Dioxide" = "SO2",
                             "Lead" = "Lead",
                             "Temperature" = "Temp",
                             "Barometric Pressure" = "Pressure"),
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
              box(width = 6,
                h1("Overview", align = "center", style = "color: #80ceff"),
                p("Open Air Chicago is an interactive dashboard providing information on air quality for the greater Chicagoland area including Milwaukee. It includes direct measures of air quality as well as variables known to affect or relate to these variables. Each of the 16 examined variables has an individual page with a variable description, source information, and interactive visualization. Additionally, the “Home” tab offers the option to explore broader trends within the data for a single variable or among several variables both at the broader Chicagoland scale and the individual county level. All data used to generate the graphs and maps on the dashboard are available for access on the “Downloads” tab.")
                ),
              box(width = 6,
                h1("Objectives", align = "center", style = "color: #c71414"),
                p("The primary goal of this dashboard is to provide both researchers and the public at large with clean, free, and easily accessible data for all things air quality. While all data used in the dashboard is technically available for free online, the numerous formats, sources, and options provide for an unwelcoming landscape. By streamlining the process through which the data is accessed, the hope is to enable more people to spend more time actually analyzing the data and working to improve air quality. Additionally, Open Air Chicago hopes to do this by offering data visualization options to explore individual variable data as well as relationships between variables over time.")
            )),
            fluidRow(
              box(width = 6,
                h1("Data", align = "center", style = "color: #c71414"),
                p("All data used to create the dashboard is available online free of charge. Sources include the Environmental Protection Agency (EPA), National Aeronautics and Space Administration (NASA),  National Oceanic and Atmospheric Association (NOAA), United States Geological Survey (USGS), and OpenStreetMap. Specific sourcing information is available on individual dashboard pages. County-level aggregates for all variables are available for download at a monthly and quarterly temporal resolution as CSV files on the “Downloads” tab. Also available on the “Downloads” tab is a GeoTiff raster file containing all of the 1km resolution gridded data.")
              ),
              box(width = 6,
                  h1("Methodology", align = "center",  style = "color: #80ceff"),
                  p("In addition to differing in source and format, the raw data also exists at a variety of spatial and temporal resolutions. All data was aggregated to a standard, 1km resolution grid at both monthly and quarterly intervals. For the EPA sensor data, the gridded values were extracted from an Inverse Distance Weighted interpolation of sensor averages. Interpolated values for variables with fewer sensors will be less accurate than those with more sensors. Due to data availability, particularly with NASA’s remote-sensed Aerosol Optical Depth, individual variable pages provide visualizations of the quarterly aggregates to maximize coverage. For data not originally provided at a 1km resolution, unless otherwise noted on the “Source” tab on each variable page, the value assigned to each 1km cell is the mean of all measured values within it.")
            ))
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

    generateQuarterlyTab(no2.tabname, no2.name, no2.description, no2.source),

    generateQuarterlyTab(o3.tabname, o3.name, o3.description, o3.source),

    generateQuarterlyTab(so2.tabname, so2.name, so2.description, so2.source),

    generateQuarterlyTab(pb.tabname, pb.name, pb.description, pb.source),

    generateOneTimeTab(pe.tabname, pe.name, pe.description, pe.source),

    generateOneTimeTab(roads.tabname, roads.name, roads.description, roads.source),

    generateQuarterlyTab(temp.tabname, temp.name, temp.description, temp.source),
    
    generateQuarterlyTab(pressure.tabname, pressure.name, pressure.description, pressure.source),

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
      setView(lat = "41.97736", lng = "-87.62255", zoom = 7) %>% 
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
      
      if(length(all.fips$fips) == 0) {
        view.lat <- 41.97736
        view.lon <- -87.62255
      }
      
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

    blues <- c("#033682", "#0356a3", "#0083d9", "#66ccff", "#c9e8ff") 
    reds <- c("#9c1500", "#f52302", "#ff6e57", "#ff9a8a", "#ffc8bf") 
    greens <- c("#165422", "#0b9926", "#14ff41", "#91faa5", "#d6ffde") 
    yellows <- c("#8f8a00", "#d4cc04", "#faf005", "#f5f190", "#faf8c3")
    grays <- c("#343d46", "#4f5b66", "#65737e", "#a7adba", "#c0c5ce") 
    
    colors <- data.frame(blues, reds, greens, yellows, grays)
    
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
                       line = list(dash = "dot", color = colors[1,i]),
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
                         line = list(color = colors[j+1,i]),
                         name = paste(county.avgs$Name[selected.fips[j]], "County", vars[i], sep = " "),
                         text= paste(county.avgs$Name[selected.fips[j]], "County", vars[i], sep = " "))
          } else {
            p <- add_trace(p,
                           x = dates,
                           y = as.numeric(county.avgs[selected.fips[j],names(these.vars.avgs[[i]])]),
                           type = "scatter",
                           mode = "lines",
                           opacity = .5,
                           line = list(color = colors[j+1, i]),
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
  
  observeEvent(input$grn_map_shape_click, {
    if(input$sidebar == "landcover") { #Optimize Dashboard speed by not observing outside of tab
      click <- input$grn_map_shape_click
      
      zoomMap("grn_map", click, large.area)
      
    }
  })
  

  output$gry_map <- renderLeaflet({
    gry.pal <- palFromLayer("gry_ndx", colors = c("white", "lightgray", "gray", "darkgray", "black"), raster = master.raster)
    gry.map <- dashMap("gry_ndx", gry.pal, raster = master.raster, area = large.area, layerId = large.area$FIPS)
  })
  
  observeEvent(input$gry_map_shape_click, {
    if(input$sidebar == "landcover") { #Optimize Dashboard speed by not observing outside of tab
      click <- input$gry_map_shape_click
      
      zoomMap("gry_map", click, large.area)
      
    }
  })
  

  output$blu_map <- renderLeaflet({
    blu.pal <- palFromLayer("blu_ndx", colors = c("white","lightblue", "blue", "darkblue"), raster = master.raster)
    blu.map <- dashMap("blu_ndx", blu.pal, raster = master.raster, area = large.area, layerId = large.area$FIPS)
  })
  
  observeEvent(input$blu_map_shape_click, {
    if(input$sidebar == "landcover") { #Optimize Dashboard speed by not observing outside of tab
      click <- input$blu_map_shape_click
      
      zoomMap("blu_map", click, large.area)
      
    }
  })
  
  
  output$elevation_map <- renderLeaflet({

    elev.pal <- palFromLayer("Elev", raster = master.raster)

    elevation.map <- dashMap("Elev", elev.pal, raster = master.raster, area = large.area, layerId = large.area$FIPS)

  })
  
  
  observeEvent(input$elevation_map_shape_click, {
    if(input$sidebar == "elevation") { #Optimize Dashboard speed by not observing outside of tab
      click <- input$elevation_map_shape_click
      
      zoomMap("elevation_map", click, large.area)
      
    }
  })
  

  output$pm25_map <- renderLeaflet({

    this.pm25.name <- "PM25_3_16"

    in.pal <- "ovr"

    pm25.pal <- palFromLayer(this.pm25.name, style = in.pal, raster = master.raster)

    dashMap(this.pm25.name, pm25.pal, 
            raster = master.raster, area = large.area, 
            layerId = large.area$FIPS, EPApoints = epa.points, 
            VarName = "PM25")
    
    
    
    
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
      if(!is.null(click$id)) {
      zoomMap("pm25_map", click, large.area)}
    }
  })

  output$pm10_map <- renderLeaflet({

    this.pm10.name <- "PM10_3_16"

    in.pal <- "ovr"

    pm10.pal <- palFromLayer(this.pm10.name, style = in.pal, raster = master.raster)

    dashMap(this.pm10.name, pm10.pal, raster = master.raster, 
            area = large.area, layerId = large.area$FIPS,
            EPApoints = epa.points, VarName = "PM10")

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
      if(!is.null(click$id)) {
      zoomMap("pm10_map", click, large.area)}
    }
  })

  output$co_map <- renderLeaflet({

    this.co.name <- "CO_3_16"

    in.pal <- "ovr"

    co.pal <- palFromLayer(this.co.name, style = in.pal, raster = master.raster)

    dashMap(this.co.name, co.pal, raster = master.raster, 
            area = large.area, layerId = large.area$FIPS,
            EPApoints = epa.points, VarName = "CO")

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
      if(!is.null(click$id)) {
      zoomMap("co_map", click, large.area)}
    }
  })

  output$no2_map <- renderLeaflet({

    this.no2.name <- "NO2_3_16"

    in.pal <- "ovr"

    no2.pal <- palFromLayer(this.no2.name, style = in.pal, raster = master.raster)

    dashMap(this.no2.name, no2.pal, raster = master.raster, 
            area = large.area, layerId = large.area$FIPS,
            EPApoints = epa.points, VarName = "NO2")

  })

  observe({
    if(input$sidebar == "no2") {
    in.date <- input$no2_dt
    this.no2.name <- getLayerName(in.date, "NO2")

    in.pal <- input$no2_rad

    no2.pal <- palFromLayer(this.no2.name, style = in.pal, raster = master.raster)

    sliderProxy("no2_map", this.no2.name, no2.pal, raster = master.raster)
    }
  })
  
  observeEvent(input$no2_map_shape_click, {
    if(input$sidebar == "no2") { 
      click <- input$no2_map_shape_click
      if(!is.null(click$id)) {
      zoomMap("no2_map", click, large.area)}
    }
  })


  output$o3_map <- renderLeaflet({

    this.o3.name <- "Ozone_3_16"
    in.pal <- "ovr"

    o3.pal <- palFromLayer(this.o3.name, style = in.pal, raster = master.raster)

    dashMap(this.o3.name, o3.pal, raster = master.raster, 
            area = large.area, layerId = large.area$FIPS,
            EPApoints = epa.points, VarName = "Ozone")

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
      if(!is.null(click$id)) {
      zoomMap("o3_map", click, large.area)}
    }
  })

  output$so2_map <- renderLeaflet({

    this.so2.name <- "SO2_3_16"

    in.pal <- "ovr"

    so2.pal <- palFromLayer(this.so2.name, style = in.pal, raster = master.raster)

    dashMap(this.so2.name, so2.pal, raster = master.raster, 
            area = large.area, layerId = large.area$FIPS,
            EPApoints = epa.points, VarName = "SO2")

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
      if(!is.null(click$id)) {
      zoomMap("so2_map", click, large.area)}
    }
  })
  
  output$pb_map <- renderLeaflet({

    this.pb.name <- "Lead_3_16"

    in.pal <- "ovr"

    pb.pal <- palFromLayer(this.pb.name, style = in.pal, raster = master.raster)

    dashMap(this.pb.name, pb.pal, raster = master.raster, 
            area = large.area, layerId = large.area$FIPS,
            EPApoints = epa.points, VarName = "Lead")

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
      if(!is.null(click$id)) {
      zoomMap("pb_map", click, large.area)}
    }
  })

  output$pe_map <- renderLeaflet({

    pe.pal <- palFromLayer("PECount", colors = c("darkgreen", "yellow2", "darkorange", "darkred"), raster = master.raster)
    pe.map <- dashMap("PECount", pe.pal, raster = master.raster, 
                      area = large.area, layerId = large.area$FIPS,
                      rasterOpacity = 0.7)

  })
  
  
  observeEvent(input$pe_map_shape_click, {
    if(input$sidebar == "pe") { #Optimize Dashboard speed by not observing outside of tab
      click <- input$pe_map_shape_click
      
      zoomMap("pe_map", click, large.area)
      
    }
  })
  

  output$roads_map <- renderLeaflet({
    roads.pal <- palFromLayer("RdDnsty", colors = c("darkgreen", "yellow2", "darkorange", "darkred"),
                              raster = master.raster)
    roads.map <- dashMap("RdDnsty", roads.pal, 
                         raster = master.raster, 
                         area = large.area, 
                         layerId = large.area$FIPS,
                         rasterOpacity = 0.5)
  })
  
  
  observeEvent(input$roads_map_shape_click, {
    if(input$sidebar == "roads") { #Optimize Dashboard speed by not observing outside of tab
      click <- input$roads_map_shape_click
      
      zoomMap("roads_map", click, large.area)
      
    }
  })
  

  output$temp_map <- renderLeaflet({
    this.temp.name <- "Temp_3_16"

    in.pal <- "ovr"

    temp.pal <- palFromLayer(this.temp.name, style = in.pal, raster = master.raster)

    dashMap(this.temp.name, temp.pal, raster = master.raster, 
            area = large.area, layerId = large.area$FIPS,
            EPApoints = epa.points, VarName = "Temp")
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
      if(!is.null(click$id)) {
      zoomMap("temp_map", click, large.area)}
    }
  })
  
  output$pressure_map <- renderLeaflet({
    this.pressure.name <- "Pressure_3_16"
    
    in.pal <- "ovr"
    
    pressure.pal <- palFromLayer(this.pressure.name, style = in.pal, raster = master.raster)
    
    dashMap(this.pressure.name, pressure.pal, raster = master.raster, 
            area = large.area, layerId = large.area$FIPS,
            EPApoints = epa.points, VarName = "Pressure")
  })
  
  observe({
    if(input$sidebar == "pressure") {
      in.date <- input$pressure_dt
      this.pressure.name <- getLayerName(in.date, "Pressure")
      
      in.pal <- input$pressure_rad
      
      pressure.pal <- palFromLayer(this.pressure.name, style = in.pal, raster = master.raster)
      
      sliderProxy("pressure_map", this.pressure.name, pressure.pal, raster = master.raster)
    }
  })
  
  observeEvent(input$pressure_map_shape_click, {
    if(input$sidebar == "pressure") { 
      click <- input$pressure_map_shape_click
      if(!is.null(click$id)) {
        zoomMap("pressure_map", click, large.area)}
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
  ### Source shapefile zip download code:
  ### https://stackoverflow.com/questions/41707760/download-a-shape-file-from-shiny
  
  output$large_area_counties <- downloadHandler(
    filename = "LargeAreaCounties.zip",
    content = function(file) {
      a <- tempdir()
      st_write(large.area, dsn = a, "LargeAreaCounties", driver = "ESRI Shapefile",
               update = TRUE, delete_layer = T)
      
      zip.path <- file.path(a, "large.area.zip")
      shp.files <- list.files(a,
                              "LargeAreaCounties", 
                              full.names = T)
      zip.cmd <- paste("zip -j",
                       zip.path,
                       paste(shp.files, collapse = " "))
      
      system(zip.cmd)
      file.copy(zip.path, file)
      file.remove(zip.path, shp.files)

    }
  )

  ##### DOWNLOADS END #####
  
  

}

shinyApp(ui, server)

