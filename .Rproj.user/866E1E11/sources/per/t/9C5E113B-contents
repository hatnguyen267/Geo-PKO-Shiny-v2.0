
#load required packages

if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
#if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(GADMTools)) install.packages("GADMTools")
if(!require(Cairo)) install.packages("Cairo")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(forcats)) install.packages("forcats")
if(!require(sp)) install.packages("sp")
if(!require(zoo)) install.packages("zoo")
if(!require(gifski)) install.packages("gifski")
if(!require(png)) install.packages("png")
if(!require(gganimate)) install.packages("gganimate")
if(!require(scales)) install.packages("scales")
if(!require(ggnewscale)) install.packages("ggnewscale")
if(!require(shinycssloaders)) install.packages("shinycssloaders")
if(!require(purrr)) install.packages("purrr")
if(!require(htmltools)) install.packages("htmltools")
if(!require(knitr)) install.packages("knitr")
if(!require(foreign)) install.packages("foreign", repos = "https://svn.r-project.org/")


options(shiny.usecairo=TRUE)

####import data####

geopko <- readr::read_csv("geopko2.csv", col_types = cols(.default="c"),
                          locale=readr::locale(encoding="latin1"))
geopko2 <- readr::read_csv("geopko2.csv", col_types = cols(.default="c"),
                           locale=readr::locale(encoding="latin1")) #version for leaflet

iso <- read.csv("geopko_ccode2.csv")

rmdfiles <- c("about.Rmd", "data.Rmd")
sapply(rmdfiles, knit, quiet=T)

####data prep TCC####

geopko <- geopko %>% 
  mutate_at(vars(c(No.troops, No.TCC, Longitude, Latitude,
                   UNMO.dummy, UNPOL.dummy)), as.numeric) %>% 
  mutate(HQ=as.factor(HQ))

tcc_df <- geopko %>% select(Source, Mission, Year, Month, 
                            No.troops, nameoftcc_1:notroopspertcc_17) %>% 
  group_by(Source, Mission, Year, Month) %>% 
  mutate(Total.troops=sum(No.troops, na.rm=T)) %>% ungroup()


#colnames(tcc_df) <- sub("name.of.TCC", "nameofTCC_", colnames(tcc_df)) obsolete script
#colnames(tcc_df) <- sub("No.troops.per.TCC", "notroopsperTCC_", colnames(tcc_df))
#tcc_df <- tcc_df %>% mutate_at(vars(starts_with("notroopsperTCC")), as.character) %>% 
#  mutate_at(vars(starts_with("nameofTCC")), as.character) %>% 

####map data prep####

cclist3 <- iso %>% select(Mission, a3) %>% distinct() #creating list of country codes for GADM sf files dowload 

map_df <- geopko %>% unite(joined_date, c("Year","Month"), sep=": ", remove=FALSE) %>% 
  unite(timepoint, c("Year","Month"), sep=" ", remove=FALSE) 

#oxford comma string
country_list <-function(w, oxford=T) {
  if(length(w)==1) return(paste("This mission was active in the following country or territory:",w));
  if(length(w)==2) return(paste("This mission was active in the following countries or territories:", w[1],"and",w[2]));
  paste0("This mission was active in the following countries or territories: ",paste(w[-length(w)], collapse=", "), 
         ifelse(oxford,",","")," and ", w[length(w)] )
}

#### data prep for leaflet ####
#data modification
geopko2$NoTroops<-as.numeric(geopko2$No.troops)
geopko2$RPF_No<-as.numeric(geopko2$RPF_No)
geopko2$UNPOL<-as.numeric(geopko2$UNPOL.dummy)
geopko2$UNMO<-as.numeric(geopko2$UNMO.dummy)
geopko2$No.TCC<-as.numeric(geopko2$No.TCC)
geopko2$Avia<-as.numeric(geopko2$Avia)
geopko2$HeSup <-as.numeric(geopko2$HeSup)
geopko2$Av<- (geopko2$Avia + geopko2$HeSup)
geopko2$Infantry <- as.numeric(geopko2$Inf_No)
geopko2$HQ <- as.numeric(geopko2$HQ)
geopko2$Reserve <- as.numeric(geopko2$RES_No)
geopko2$Longitude<-as.numeric(geopko2$Longitude)
geopko2$Latitude <- as.numeric(geopko2$Latitude)
#markers

HQicon <- awesomeIcons(
  icon = 'fas fa-home',
  markerColor = "black",
  iconColor = "#f7fcff",
  library = 'fa'
)

Medicon <- awesomeIcons(
  icon = 'fas fa-plus',
  markerColor = "white",
  iconColor = "red",
  library = 'fa'
)

UNPOLicon <- awesomeIcons(
  icon = 'fab fa-product-hunt',
  markerColor = "blue",
  iconColor = "#f6f6f6",
  library = 'fa'
)
UNMOicon <- awesomeIcons(
  icon = 'fas fa-binoculars',
  markerColor = "darkblue",
  iconColor = "#f6f6f6",
  library = 'fa'
)

Avicon <- makeAwesomeIcon(
  icon = 'fas fa-plane',
  markerColor = "gray",
  iconColor = "#ffffff",
  squareMarker = FALSE,
  library = 'fa'
)

Rivicon <- makeAwesomeIcon(
  icon = 'fas fa-anchor',
  markerColor = "cadetblue",
  iconColor = "#ffffff",
  squareMarker = FALSE,
  library = 'fa'
)

Engicon <- makeAwesomeIcon(
  icon = 'fas fa-bolt',
  markerColor = "black",
  iconColor = "#ffffff",
  squareMarker = FALSE,
  library = 'fa'
)

Sigicon <- makeAwesomeIcon(
  icon = 'fas fa-wifi',
  markerColor = "lightgray",
  iconColor = "#ffffff",
  squareMarker = FALSE,
  library = 'fa'
)

Traicon <- makeAwesomeIcon(
  icon = 'fas fa-truck',
  markerColor = "darkpurple",
  iconColor = "#ffffff",
  squareMarker = FALSE,
  library = 'fa'
)

Mainticon <- makeAwesomeIcon(
  icon = 'fas fa-wrench',
  markerColor = "darkgreen",
  iconColor = "#ffffff",
  squareMarker = FALSE,
  library = 'fa'
)

#basemaps
FrontmapData <- geopko2 %>% select(Mission, Year, Country, Location, Latitude, Longitude, Infantry, NoTroops, Reserve, HQ, UNPOL, Med,Av,UNMO) %>%
  group_by(Mission, Year, Location, Country) %>% 
  mutate(Av = max(Av, na.rm=TRUE))%>%
  mutate(Med = max(Med, na.rm=TRUE))%>% 
  mutate(Infantry = as.integer(mean(Infantry, na.rm=TRUE)))%>%
  mutate(Reserve = as.integer(mean(Reserve, na.rm=TRUE)))%>%
  mutate(UNPOL = as.integer(mean(UNPOL, na.rm=TRUE)))%>% 
  mutate(UNMO = max(UNMO, na.rm=TRUE))%>% 
  mutate(ave.no.troops = as.integer(mean(NoTroops, na.rm=TRUE))) %>% 
  select(-NoTroops) %>% distinct() %>% drop_na(ave.no.troops)

FrontmapData$UNPOL <- str_replace_all(FrontmapData$UNPOL, "-Inf", "0")
FrontmapData$UNPOL <- as.numeric(FrontmapData$UNPOL)

###TCC dataframe
TCCmapData <- geopko2 %>% select(Source:Location, Latitude, Longitude,
                                 No.TCC:notroopspertcc_17, HQ)

TCCmapData <- TCCmapData %>% pivot_longer(nameoftcc_1:notroopspertcc_17,
                                        names_to=c(".value", "TCC_id"),
                                        names_sep="_") %>%
  filter(!is.na(nameoftcc)) %>%  #dropping empty tcc name cells
  mutate_at(vars(notroopspertcc), as.numeric) %>%
  group_by(Mission, Year, Location, Latitude, Longitude, nameoftcc) %>%
  summarise(count.per.tcc.year=as.character(max(notroopspertcc))) %>%
  mutate(count.per.tcc.year=ifelse(is.na(count.per.tcc.year), "unknown", count.per.tcc.year),
         single.tcc=paste0(nameoftcc, " (",count.per.tcc.year,(")"))) %>%
  add_count(Year, Location, name="No.TCC")%>%
  group_by(Mission, Year, Location, Latitude, Longitude, No.TCC) %>%
  summarise(year.overview = str_c(single.tcc, collapse=", "))
##Troop Type Dataframe
TTmapData <- geopko2 %>% select(Source:Location, Latitude, Longitude, Infantry,
                                Eng:MP) %>%group_by(Mission, Year, Location)%>% mutate(Infantry = as.integer(mean(Infantry, na.rm=TRUE)))%>% distinct()

###Legend colours
ColoursFrontmap <- colorBin(rev(viridis::viridis(10)), 
                            FrontmapData$ave.no.troops, 
                            bins = c(10,50,100,500,1000,2000,4000,8000))
ColoursTCCmap <- colorBin((viridis::viridis(2)), 
                          TCCmapData$No.TCC, 
                          bins = c(1,2,4,7,10,15,20))
ColoursTTmap <- colorBin(rev(viridis::viridis(10)), 
                         TTmapData$Infantry, 
                         bins = c(10,50,100,500,1000,2000,4000,8000))
####Map doesnt load on initial go, so need to make the base here
TCC_basemap <- leaflet(geopko2, options = leafletOptions(minZoom = 2)) %>% 
  addTiles()  %>% 
  hideGroup("Mission HQ")%>%
  fitBounds(~-70,-50,~60,60) %>%
  addLegend(pal = ColoursTCCmap, values = ~TCCmapData$No.TCC, group = "TCC", title= "Number of TCCs") %>%
  addCircleMarkers(data= (DataCircleTCC<-TCCmapData%>%filter(Year==2020)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC), 
                   fillOpacity = 0.8, color = ~ColoursTCCmap(No.TCC), group = "TCC", labelOptions = labelOptions(style= list(
                     "width"= "150px", "white-space"="normal")),
                   label = paste("<strong>", DataCircleTCC$Mission,"</strong><br/><strong>Location:</strong>",DataCircleTCC$Location,"<br/><strong>Total number of TCCs:</strong>",DataCircleTCC$No.TCC, "<br/><strong>Countries:</strong>",DataCircleTCC$year.overview)%>% lapply(htmltools::HTML))

####BaseMap Third Panel
TroopType_basemap <- leaflet(geopko2, options = leafletOptions(minZoom = 2)) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    baseGroups = c("Infantry", "None"),
    overlayGroups = c("Medical", "Engineering", "Signals", "Aviation", "Transport", "Maintenance", "Riverine"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("Medical","Aviation", "Engineering", "Transport", "Signals","Maintenance","Riverine"))%>%
  fitBounds(~-70,-50,~60,60) %>%
  addLegend(pal = ColoursTTmap, values = ~TTmapData$Infantry, group = "Infantry", title= "Number of troops") %>%
  addCircleMarkers(data=(gif_df31<-TTmapData%>%filter(Year==2020)%>%filter(Infantry>1)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(Infantry)^(1/3.5), 
                   fillOpacity = 0.6, color = ~ColoursTTmap(Infantry), group = "Infantry", 
                   label = paste("<strong>Location:</strong>",gif_df31$Location)%>% lapply(htmltools::HTML))%>%
  addAwesomeMarkers(data = (gif_df32<-TTmapData%>%filter(Year==2020)%>%filter(Med>0)), lat = ~Latitude+0.2, lng = ~Longitude+0.2, icon = Medicon, group = "Medical",
                    label=paste("<strong>Medical</strong><br/>", gif_df32$Mission," (",gif_df32$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
  addAwesomeMarkers(data = (gif_df33<-TTmapData%>%filter(Year==2020)%>%filter(Eng>0)), lat = ~Latitude, lng = ~Longitude, icon = Engicon, group = "Engineering",
                    label=paste("<strong>Engineering</strong><br/>", gif_df33$Mission," (",gif_df33$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
  addAwesomeMarkers(data = (gif_df35<-TTmapData%>%filter(Year==2020)%>%filter(Sig>0)), lat = ~Latitude-0.2, lng = ~Longitude-0.2, icon = Sigicon, group = "Signals",
                    label=paste("<strong>Signal</strong><br/>", gif_df35$Mission," (",gif_df35$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
  addAwesomeMarkers(data = (gif_df34<-TTmapData%>%filter(Year==2020)%>%filter(Avia>0)), lat = ~Latitude+0.4, lng = ~Longitude+0.4, icon = Avicon, group = "Aviation",
                    label=paste("<strong>Aviation</strong><br/>", gif_df34$Mission," (",gif_df34$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
  addAwesomeMarkers(data = (gif_df36<-TTmapData%>%filter(Year==2020)%>%filter(Riv>0)), lat = ~Latitude-0.6, lng = ~Longitude-0.6, icon = Rivicon, group = "Riverine",
                    label=paste("<strong>Riverine</strong><br/>", gif_df36$Mission,"(",gif_df36$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
  addAwesomeMarkers(data = (gif_df37<-TTmapData%>%filter(Year==2020)%>%filter(Maint>0)), lat = ~Latitude-0.4, lng = ~Longitude-0.4, icon = Mainticon, group = "Maintenance",
                    label=paste("<strong>Maintenance</strong><br/>", gif_df37$Mission," (",gif_df37$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
  addAwesomeMarkers(data = (gif_df38<-TTmapData%>%filter(Year==2020)%>%filter(Trans>0)), lat = ~Latitude+0.6, lng = ~Longitude+0.6, icon = Traicon, group = "Transport",
                    label=paste("<strong>Transport</strong><br/>", gif_df38$Mission," (",gif_df38$Location,")<br/>")%>% lapply(htmltools::HTML))


####UI####

ui <- fluidPage(
  navbarPage("Geo-PKO",
             navbarMenu("Troop Deployments",
                        tabPanel("Overview",tags$style(type = "text/css", "#basemap {height: calc(100vh - 130px) !important;}"), leafletOutput("basemap"),
                                 absolutePanel(top = 70, left = 85, width="20%", style = "padding: 16px; background:rgba(232, 232, 232, 0.8)",
                                               span(h6("This interactive map shows peacekeeping deployments from 1994-2020, based on publicly available United Nations (UN) peacekeeping deployment maps and mission progress reports. 'Mission Site' indicates where there are no active troop deployments, but the presence of support personnel such as UNPOL (UN Police) and/or UNMO (UN Military Observer).", align = "Left"), style="color:#15110d"),
                                               br(),
                                               span(h5(tags$b(textOutput("reactive_year"), 
                                                              align = "left"), 
                                                       style="color:#15110d")),
                                               span(h4(textOutput("reactive_troopcount"), 
                                                       align = "left"), 
                                                    style="color:#15110d"),
                                               span(h4(textOutput("reactive_UNPOLcount"), 
                                                       align = "left"), 
                                                    style="color:#666666"),
                                               span(h4(textOutput("reactive_UNMOcount"), align = "left"), style="color:#666666"),
                                               br(),
                                               pickerInput("missions","Select mission(s)", choices=as.character(unique(FrontmapData$Mission)),selected =as.character(unique(FrontmapData$Mission)) , options = list(`actions-box` = TRUE),multiple = T),
                                               chooseSliderSkin("Shiny", color = "transparent"),
                                               setSliderColor("transparent", 1),
                                               br(),
                                               sliderInput(inputId = "plot_date", 
                                                           label = "Select deployment year (1994-2020)",
                                                           min = 1994,
                                                           max = 2020,
                                                           value =2020,
                                                           step = 1,
                                                           sep= "",
                                                           animate = animationOptions(interval = 1500, loop = TRUE)), tags$style(type= "text/css", HTML(".irs-single {color:black; background:transparent}"))
                                 )),
                        tabPanel("Contributing Countries",
                                 sidebarLayout(sidebarPanel( "This map shows how many troop-contributing countries (TCCs) have deployed peacekeepers to a location. TCCs and the number of troops each country has contributed are shown in the labels.<br/><br/>"%>% lapply(htmltools::HTML),
                                                             chooseSliderSkin("Shiny", color = "transparent"),
                                                             setSliderColor("transparent", 1),
                                                             sliderInput(inputId = "plot_date2", 
                                                                         label = "Select year (1994-2020)",
                                                                         min = 1994,
                                                                         max = 2020,
                                                                         value =2020,
                                                                         step = 1,
                                                                         sep= "",
                                                                         width = "100%",
                                                                         animate = animationOptions(interval = 2000, loop = TRUE)),
                                                             pickerInput("missions2","Select mission(s)", 
                                                                         choices=as.character(unique(TCCmapData$Mission)),
                                                                         selected =as.character(unique(TCCmapData$Mission)) , 
                                                                         options = list(`actions-box` = TRUE),multiple = T), width = 3),
                                               mainPanel ( tags$style(type = "text/css", "#map {height: calc(100vh - 130px) !important;}"),leafletOutput("map",width = "115%")),
                                               position = c("left", "right")
                                 )),
                        tabPanel("Troop Types",
                                 sidebarLayout(sidebarPanel(paste ("This map shows the various types of troops deployed to a location. In the dataset, a specific number of deployed troops is provided for infantry. For other troop types, it is only indicated whether they are present or absent. Aviation is coded for aircraft, UAVs and helicopter support.<br/><br/>When selecting different troop types, overlap can occur. If no icons appear when selecting a troop type, this type is not present in the selected year.<br/><br/>")%>% lapply(htmltools::HTML),
                                                            chooseSliderSkin("Shiny", color = "transparent"),
                                                            setSliderColor("transparent", 1),
                                                            sliderInput(inputId = "plot_date3", 
                                                                        label = "Select year (1994-2020)",
                                                                        min = 1994,
                                                                        max = 2020,
                                                                        value =2020,
                                                                        step = 1,
                                                                        sep= "",
                                                                        width = "100%",
                                                                        animate = animationOptions(interval = 2000, loop = TRUE)),
                                                            pickerInput("missions3","Select mission(s)", 
                                                                        choices=as.character(unique(TTmapData$Mission)),
                                                                        selected =as.character(unique(TTmapData$Mission)) , 
                                                                        options = list(`actions-box` = TRUE),multiple = T), width = 3),
                                               mainPanel(tags$style(type = "text/css", 
                                                                    "#TroopTypeMap {height: calc(100vh - 130px) !important;}"), 
                                                         leafletOutput("TroopTypeMap", width = "115%")),
                                               position = c("left", "right")))),
             ####Screen size, responsive to different types
             
             navbarMenu("Map Generator",
                        tabPanel("Static Maps", fluid=TRUE,
                                 titlePanel("Static Maps"),
                                 sidebarLayout(
                                   sidebarPanel(width=3, 
                                                p("Where are UN peacekeepers posted, and how many? Select the options below to visualise."),
                                                selectInput(inputId="mission_map", label="Select a mission",
                                                            choices=factor(geopko$Mission), width=150),
                                                selectInput("timepoint_map", 
                                                            "Choose year and month", choices=NULL),
                                                # checkboxInput(inputId="depsize_map", 
                                                #               "Deployment size", value=TRUE),
                                                checkboxInput(inputId="MHQ_map", 
                                                              "Mission HQ", value=FALSE),
                                                checkboxInput(inputId="SHQ_map", 
                                                              "Sector HQ", value=FALSE),
                                                checkboxInput(inputId="MO_map", 
                                                              "UNMO", value=FALSE),
                                                checkboxInput(inputId="UNPOL_map", 
                                                              "UNPOL", value=FALSE),
                                                helpText("Errors may occur when a selected feature is not available for a map. If that happens, please deselect the option.")
                                   ),
                                   mainPanel(
                                     withSpinner(plotOutput("depmap", height="auto")),
                                     span(h6(textOutput("basecountries"), align="center")),
                                     hr(),
                                     fluidRow(
                                       DT::dataTableOutput(outputId="map_df_details")
                                     )
                                     
                                   )
                                 )
                        ),
                        ####animated maps UI####
                        tabPanel("Animated Maps", fluid=TRUE,
                                 titlePanel("Animated Maps"),
                                 sidebarLayout(
                                   sidebarPanel(width=3, 
                                                p(""),
                                                selectInput(inputId="anim_map", label="Animated maps show changes over time. Select a mission to visualise.",
                                                            choices=factor(geopko$Mission), width=200)
                                                
                                   ),
                                   mainPanel(fluid=TRUE, 
                                             withSpinner(imageOutput("animated")))))),
             tabPanel("Contributing Countries",
                      basicPage(
                        radioButtons(inputId="databy_tcc", 
                                     label="Present data by:", 
                                     choices=c("Deployment map", "Year"),
                                     selected="Deployment map (default)"),
                        helpText("The Geo-PKO dataset collects data by deployment maps published by the UN. For the best accuracy, display data by deployment maps. Data by year present the year's average troop counts and the highest number of troop-contributing countries (TCCs)."),
                        # span(h6(textOutput("tabletext", align="right"))),
                        DT::dataTableOutput("tcc_table")
                      )),
             tabPanel("Missions",
                      sidebarLayout(
                        sidebarPanel(
                          p("Which locations had peacekeepers deployed, and when? These graphs show the years for each mission in which a location had at least one active deployment."),
                          selectInput(inputId="Lollipop_map", label="Select a mission",
                                      choices=factor(Years$Mission), width=150), width= 3
                        ),
                        mainPanel(fluid=TRUE,
                                  plotOutput("lollipop", height="auto")
                        )
                      )),
             tabPanel ("Data",tags$div(
               withMathJax(includeMarkdown("data.md"))
             ), style='width:1100px'),
             tabPanel ("About",tags$div(
               withMathJax(includeMarkdown("about.md"))
             ), style='width:1100px'))
)



server <- function(input, output, session){
  ####Leaflet reactive dataframe#### 
  filteredData <- reactive({
    FrontmapData %>% filter(Mission %in% input$missions & Year %in% input$plot_date)
  })
  
  filteredDataTCC <- reactive({
    TCCmapData %>% filter(Mission %in% input$missions2 & Year %in% input$plot_date2)
  })
  
  filteredDataTroopType <- reactive({
    TTmapData %>% filter(Mission %in% input$missions3 & Year %in% input$plot_date3)
  })
  
  #####Front map basis  
  output$basemap <- renderLeaflet({
    leaflet(geopko2, options = leafletOptions(minZoom = 2)) %>% 
      addTiles() %>% 
      addLayersControl(
        position = "bottomright",
        baseGroups = c("Deployments (All)", "Troops (Infantry)","Troops (Reserve)","Mission Site (No Troops)","None"),
        overlayGroups = c("UNPOL", "UNMO", "Mission HQs"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup(c("UNPOL", "UNMO", "Mission HQs"))  %>%
      fitBounds(~-70,-50,~60,60) %>%
      addLegend(pal = ColoursFrontmap, values = ~FrontmapData$ave.no.troops, group = "Troop deployment", title= "Number of troops")
  })
  
  
  ####Map for TCC  
  output$map <- renderLeaflet({
    TCC_basemap
  })
  
  ####Map for Troop Types 
  output$TroopTypeMap <- renderLeaflet({
    TroopType_basemap
  })
  
  #Reactive Text for the front page
  output$reactive_year <- renderText({
    paste0("In ",unique(filteredData()$Year), " there were:")
  }) 
  
  output$reactive_troopcount <- renderText({
    paste0(prettyNum(sum(filteredData()$ave.no.troops), big.mark=","), "  peacekeepers deployed")
  }) 
  
  output$reactive_UNPOLcount <- renderText({
    paste0(prettyNum(sum(filteredData()$UNPOL, na.rm=TRUE), big.mark=","), " UNPOL deployments")
  }) 
  
  output$reactive_UNMOcount <- renderText({
    paste0(prettyNum(sum(filteredData()$UNMO, na.rm=TRUE), big.mark=","), " UNMO deployments")
  })
  
  
  
  
  ###Generate the troop deployment map
  observe({
    leafletProxy(mapId = "basemap", data = filteredData()) %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(data = (filteredData1<-filteredData()%>%filter(ave.no.troops>0)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(ave.no.troops)^(1/3.5), 
                       fillOpacity = 0.6, color = ~ColoursFrontmap(ave.no.troops), group = "Deployments (All)", 
                       label=paste("<strong>Troop number:</strong>", filteredData1$ave.no.troops,"<br/><strong>Mission:</strong>", filteredData1$Mission,"<br/><strong>Location:</strong>",filteredData1$Location)%>% lapply(htmltools::HTML)) %>%
      addCircleMarkers(data = (filteredData10<-filteredData()%>%filter(ave.no.troops==0)), lat = ~Latitude, lng = ~Longitude, weight = 0.5, radius = 3, 
                       fillOpacity = 0.4, color = "#666666", group = "Deployments (All)", 
                       label=paste("<strong>Mission site (no troop deployment)</strong><br/><strong>Mission:</strong>", filteredData10$Mission,"<br/><strong>Location:</strong>",filteredData10$Location)%>% lapply(htmltools::HTML)) %>%
      addCircleMarkers(data = (filteredData7<-filteredData()%>%filter(Infantry>0)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(ave.no.troops)^(1/3.5), 
                       fillOpacity = 0.6, color = ~ColoursFrontmap(Infantry), group = "Troops (Infantry)", 
                       label=paste("<strong>Troop number:</strong>", filteredData7$Infantry,"<br/><strong>Mission:</strong>", filteredData7$Mission,"<br/><strong>Location:</strong>",filteredData7$Location)%>% lapply(htmltools::HTML)) %>%
      addCircleMarkers(data = (filteredData8<-filteredData()%>%filter(ave.no.troops==0)), lat = ~Latitude, lng = ~Longitude, weight = 0.5, radius = 3, 
                       fillOpacity = 0.4, color = "#666666", group = "Mission Site (No Troops)", 
                       label=paste("<strong>Mission site (no troop deployment)</strong><br/><strong>Mission:</strong>", filteredData8$Mission,"<br/><strong>Location:</strong>",filteredData8$Location)%>% lapply(htmltools::HTML)) %>%
      addCircleMarkers(data = (filteredData9<-filteredData()%>%filter(Reserve>0)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(Reserve)^(1/3.5), 
                       fillOpacity = 0.6, color = ~ColoursFrontmap(Infantry), group = "Troops (Reserve)", 
                       label=paste("<strong>Troop number:</strong>", filteredData9$Reserve,"<br/><strong>Mission:</strong>", filteredData9$Mission,"<br/><strong>Location:</strong>",filteredData9$Location)%>% lapply(htmltools::HTML)) %>%
      addAwesomeMarkers(data = (filteredData2<-filteredData()%>%filter(UNPOL>0)), lat = ~Latitude, lng = ~Longitude,icon=UNPOLicon, group = "UNPOL", 
                        label=paste("<strong>UNPOL</strong> (",filteredData2$Mission,")<br/><strong>Location:</strong>",filteredData2$Location)%>% lapply(htmltools::HTML)) %>%
      addAwesomeMarkers(data = (filteredData3<-filteredData()%>%filter(UNMO>0)), lat = ~Latitude, lng = ~Longitude, icon=UNMOicon, group = "UNMO", 
                        label=paste("<strong>UNMO <br/>Mission:</strong>", filteredData3$Mission,"<br/><strong>Location:</strong>",filteredData3$Location)%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredData4<-filteredData()%>%filter(HQ==3)), lat = ~Latitude, lng = ~Longitude, icon = HQicon, group = "Mission HQs", 
                        label=paste("<strong>Mission HQ:</strong>", filteredData4$Mission,"<br/><strong>Location:</strong>",filteredData4$Location,filteredData4$Country)%>% lapply(htmltools::HTML))
  })
  
  
  ####Second observe for TCC map   
  observe({
    leafletProxy(mapId = "map", data = filteredDataTCC()) %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(data = filteredDataTCC(), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1.5), 
                       fillOpacity = 0.6, color = ~ColoursTCCmap(No.TCC), group = "TCC", labelOptions = labelOptions(style= list(
                         "width"= "150px", "white-space"="normal")),
                       label = paste("<strong>", filteredDataTCC()$Mission,"</strong><br/><strong>Location:</strong>",filteredDataTCC()$Location, "<br/><strong>Total number of TCCs:</strong>",filteredDataTCC()$No.TCC,"<br/><strong>Countries:</strong>",filteredDataTCC()$year.overview)%>% lapply(htmltools::HTML))
  })
  
  
  ####Third observe for Troop Type map   
  observe({
    leafletProxy(mapId = "TroopTypeMap", data = filteredDataTroopType()) %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(data = (filteredDataTroopType0<-filteredDataTroopType()%>%filter(Infantry>0)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(Infantry)^(1/3.5), 
                       fillOpacity = 0.6, color = ~ColoursTTmap(Infantry), group = "Infantry", 
                       label = paste("<strong>Location:</strong>",filteredDataTroopType()$Location)%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredDataTroopType1<-filteredDataTroopType()%>%filter(Med>0)), lat = ~Latitude+0.2, lng = ~Longitude+0.2, icon = Medicon, group = "Medical",
                        label=paste("<strong>Medical</strong><br/>", filteredDataTroopType1$Mission," (",filteredDataTroopType1$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredDataTroopType2<-filteredDataTroopType()%>%filter(Eng>0)), lat = ~Latitude, lng = ~Longitude, icon = Engicon, group = "Engineering",
                        label=paste("<strong>Engineering</strong><br/>", filteredDataTroopType2$Mission," (",filteredDataTroopType2$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredDataTroopType4<-filteredDataTroopType()%>%filter(Sig>0)), lat = ~Latitude-0.2, lng = ~Longitude-0.2, icon = Sigicon, group = "Signals",
                        label=paste("<strong>Signal</strong><br/>", filteredDataTroopType4$Mission," (",filteredDataTroopType4$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredDataTroopType3<-filteredDataTroopType()%>%filter(Avia>0)), lat = ~Latitude+0.4, lng = ~Longitude+0.4, icon = Avicon, group = "Aviation",
                        label=paste("<strong>Aviation</strong><br/>", filteredDataTroopType3$Mission," (",filteredDataTroopType3$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredDataTroopType5<-filteredDataTroopType()%>%filter(Riv>0)), lat = ~Latitude-0.6, lng = ~Longitude-0.6, icon = Rivicon, group = "Riverine",
                        label=paste("<strong>Riverine</strong><br/>", filteredDataTroopType5$Mission,"(",filteredDataTroopType5$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredDataTroopType6<-filteredDataTroopType()%>%filter(Maint>0)), lat = ~Latitude-0.4, lng = ~Longitude-0.4, icon = Mainticon, group = "Maintenance",
                        label=paste("<strong>Maintenance</strong><br/>", filteredDataTroopType6$Mission," (",filteredDataTroopType6$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredDataTroopType7<-filteredDataTroopType()%>%filter(Trans>0)), lat = ~Latitude+0.6, lng = ~Longitude+0.6, icon = Traicon, group = "Transport",
                        label=paste("<strong>Transport</strong><br/>", filteredDataTroopType7$Mission," (",filteredDataTroopType7$Location,")<br/>")%>% lapply(htmltools::HTML))
  })
  #TCC tables
  bymap_df <- reactive({
    tcc_df %>% 
      pivot_longer(c(nameoftcc_1:notroopspertcc_17), names_to=c(".value", "tcc_id"), names_sep="_") %>%
      filter(!is.na(nameoftcc)) %>%
      mutate_at(vars(notroopspertcc), as.numeric) %>% 
      select(-tcc_id) %>% 
      group_by(Source, Mission, Year, Month, Total.troops, nameoftcc)%>%
      summarise(total.tcc=as.character(sum(notroopspertcc), na.rm=TRUE)) %>% 
      add_count(Source, name="No.TCC") %>%
      mutate(total.tcc=ifelse(is.na(total.tcc), "size unknown", total.tcc), 
             overview=paste0(nameoftcc," (",total.tcc,")")) %>%
      group_by(Source, Mission, Year, Month, Total.troops, No.TCC) %>%
      summarise(details=str_c(overview, collapse=", ")) %>% 
      arrange(desc(Year))
  }) 
  
  byyear_df <- reactive({
    tcc_df %>% 
      pivot_longer(c(nameoftcc_1:notroopspertcc_17), names_to=c(".value", "TCC_id"), names_sep="_")%>%
      filter(!is.na(nameoftcc)) %>%
      mutate_at(vars(notroopspertcc), as.numeric) %>% 
      select(-TCC_id) %>% 
      group_by(Source, Mission, Year, Month, Total.troops, nameoftcc)%>%
      summarise(total.each.tcc=as.character(sum(notroopspertcc, na.rm=TRUE))) %>% 
      add_count(Source, name="No.TCC") %>%
      mutate(total.each.tcc=ifelse(total.each.tcc=="0","size unknown", total.each.tcc),
             overview=paste0(nameoftcc," (",total.each.tcc,")")) %>%
      select(-nameoftcc, -total.each.tcc) %>%
      group_by(Source, Mission, Year, Month, Total.troops, No.TCC) %>%
      summarise(byyear.overview=str_c(overview, collapse=", ")) %>% 
      arrange(desc(Year)) %>%
      group_by(Mission, Year) %>% mutate(min.troops= as.character(min(Total.troops)),
                                         max.troops=as.character(max(Total.troops)),
                                         ave.troops=as.character(round(mean(Total.troops)))) %>%
      group_by(Mission, Year) %>% arrange(desc(No.TCC)) %>% dplyr::slice(1) %>% 
      mutate(ave.troops=ifelse(is.na(ave.troops), "Unknown", ave.troops),
             min.troops=ifelse(is.na(min.troops), "Unknown", min.troops),
             max.troops=ifelse(is.na(max.troops), "Unknown", max.troops)) %>%
      select(Mission, Year, No.TCC, byyear.overview, min.troops, max.troops, ave.troops)
  })
  
  # output$tabletext <- renderText({
  #   req(input$databy_tcc)
  #   if(input$databy_tcc=="Deployment map"){
  #   paste("")  
  #   }
  #   else if(input$databy_tcc=="Year"){
  #   paste("Data collected")
  #   }
  # })
  #   
  output$tcc_table <- DT::renderDataTable({
    req(input$databy_tcc)
    if(input$databy_tcc=="Deployment map"){
      DT::datatable(bymap_df(),
                    colnames = c("Source map", "Mission", "Year", "Month", 
                                 "Total Troop Count", "Number of TCCs", 
                                 "Details"),
                    rownames = FALSE)
    }
    else if(input$databy_tcc=="Year"){
      DT::datatable(byyear_df(),
                    colnames = c("Mission", "Year", "Number of TCCs", "Details",
                                 "Min. Troop Count", "Max. Troop Count", "Mean Troop Count"),
                    rownames= FALSE)
    }
  })
  
  ####deployment maps####
  
  #creating list of sf objects to download
  sfdf <- reactive({
    req(input$mission_map)
    cclist3 %>% filter(Mission %in% input$mission_map)
  })
  
  observeEvent(input$mission_map,{
    updateSelectInput(session, 'timepoint_map',
                      choices = unique(map_df$joined_date[map_df$Mission==input$mission_map]))
    
  })
  
  map_df_temp <- reactive({
    req(input$mission_map)
    req(input$timepoint_map)
    map_df %>% filter(Mission %in% input$mission_map) %>%
      filter(joined_date %in% input$timepoint_map)
    
  })
  
  map_zero <- reactive({
    map_df_temp() %>% filter(No.troops==0, No.TCC==0)
  })
  
  output$basecountries <- renderText({
    unique_country <- unique(map_df_temp()$Country)
    country_list(unique_country)
    # if(length(countrieslist)<=2){
    # paste0("This mission took place in: ",paste0(unique(map_df_temp()$Country), collapse=" and "),".")}
    # else{
    #   paste0("This mission took place in ",paste0(unique(map_df_temp()$Country), collapse=" and "),".")  
    # }
  })
  
  UNMO_df_temp <- reactive({
    map_df_temp() %>% filter(UNMO.dummy==1)
  })
  
  UNPOL_df_temp <- reactive({
    map_df_temp() %>% filter(UNPOL.dummy==1)
  })
  
  SHQ_df_temp <- reactive({
    map_df_temp() %>% filter(HQ==2)
  })
  
  g <- guide_legend("title")
  output$depmap <- renderPlot({
    input$depsize_map
    input$MHQ_map
    input$SHQ_map
    input$MO_map
    input$UNPOL_map
    
    maplist <- pull(sfdf(), a3)
    mapshapefiles <- gadm_sf_loadCountries(c(paste(maplist)), level=1)
    
    p <- ggplot() + geom_sf(data=mapshapefiles$sf, fill="grey80") + 
      theme_void() + 
      labs(title=paste(map_df_temp()$Mission,": ", map_df_temp()$timepoint),
           caption="Sources: Geo-PKO v2.0\n Shapefiles from GADM.")+
      geom_blank()+
      geom_point(data=map_df_temp(), 
                 aes(x=Longitude, y=Latitude, shape="Blank", color="Blank"),
                 size=2, stroke=0.7, fill="grey44")+
      scale_shape_manual(values=c("Blank"=22),
                         labels=c("Blank"="Mission sites"),
                         name="")+
      scale_color_manual(values=c("Blank"="grey44"),
                         labels=c("Blank"="Mission sites"),
                         name="")+
      new_scale_color()+
      new_scale("shape")+
      geom_point(data=map_df_temp() %>% filter(No.troops>0 | No.TCC>0),
                 aes(x=Longitude, y=Latitude, size=No.troops, color=as.integer(No.TCC)),
                 shape=20, alpha = 0.8)+
      scale_size_binned(name="Size of deployment",range=c(2, 16))+
      {if(max(map_df_temp()$No.TCC)<=4)list(
        scale_color_continuous(low = "thistle3", high = "darkred",
                               guide="colorbar", name="No. of Troop-\nContributing Countries",
                               breaks=c(1,2,3,4),
                               limits=c(1,4)))
      } +
      {if(max(map_df_temp()$No.TCC)>4)list(
        scale_color_continuous(low = "thistle3", high = "darkred",
                               guide="colorbar", name="No. of Troop-\nContributing Countries",
                               breaks=pretty_breaks())
      )}+
      new_scale_color()+
      scale_shape_manual(values=c("SHQ"=3,
                                  "UNMO"=24,
                                  "UNPOL"=23),
                         labels=c("SHQ"="Sector HQ", "UNMO"="Military Observers", "UNPOL"="UN Police"),
                         name="")+
      scale_color_manual(values=c("SHQ"="orange",
                                  "UNMO"="darkblue",
                                  "UNPOL"="darkgreen"),
                         labels=c("SHQ"="Sector HQ", "UNMO"="Military Observers", "UNPOL"="UN Police"),
                         name="")
    
    
    # if(input$depsize_map){
    #   if(nrow(map_zero()) >0){
    #     p <- p + 
    #       geom_point(data=map_df_temp() %>% filter(No.troops>0 | No.TCC>0), 
    #                  aes(x=Longitude, y=Latitude, size=No.troops, color=as.integer(No.TCC)),
    #                  shape=20, alpha = 0.8)+
    #       scale_size_binned(name="Size of deployment",range=c(2, 16))+
    #       {if(max(map_df_temp()$No.TCC)<=4)list(
    #         scale_color_continuous(low = "thistle3", high = "darkred", 
    #                                guide="colorbar", name="Number of TCCs",
    #                                breaks=c(1,2,3,4),
    #                                limits=c(1,4)))}+
    #       {if(max(map_df_temp()$No.TCC)>4)list(
    #         scale_color_continuous(low = "thistle3", high = "darkred", 
    #                                guide="colorbar", name="Number of TCCs",
    #                                breaks=pretty_breaks()))}+
    #       new_scale_color()+
    #       
    #       geom_point(data=map_df_temp() %>% filter(No.troops==0, No.TCC==0), 
    #                  aes(x=Longitude, y=Latitude, shape="Blank", color="Blank"), 
    #                  size=2, stroke=0.5)+
    #       scale_shape_manual(values=c("Blank"=22),
    #                          labels=c("Blank"="Locations with no troops recorded"),
    #                          name="")+
    #       
    #       scale_color_manual(values=c("Blank"="grey44"),
    #                          labels=c("Blank"="Locations with no troops recorded"),
    #                          name="")+
    #       # guides(shape=guide_legend(title="", order=3), color=guide_legend(title="", order=3))+
    #       new_scale_color()+
    #       new_scale("shape")+
    #       scale_shape_manual(values=c("SHQ"=3,
    #                                   "UNMO"=24,
    #                                   "UNPOL"=23),
    #                          labels=c("SHQ"="Sector HQ", "UNMO"="Military Observers", "UNPOL"="UN Police"),
    #                          name="Non-combat functions")+
    #       scale_color_manual(values=c("SHQ"="orange",
    #                                   "UNMO"="darkblue",
    #                                   "UNPOL"="darkgreen"),
    #                          labels=c("SHQ"="Sector HQ", "UNMO"="Military Observers", "UNPOL"="UN Police"),
    #                          name="Non-combat functions")
    #   }
    #   else{
    #     p <- p + geom_point(data=map_df_temp(), 
    #                         aes(x=Longitude, y=Latitude, size=No.troops, color=as.integer(No.TCC)),
    #                         shape=20, alpha = 0.8)+
    #       scale_size_binned(name="Size of deployment",range=c(2, 16))+
    #       #  scale_color_brewer(palette="Set1", name="Number of TCCs")+
    #       {if(max(map_df_temp()$No.TCC)<=4)list(
    #         scale_color_continuous(low = "thistle3", high = "darkred", 
    #                                guide="colorbar", name="Number of TCCs",
    #                                breaks=c(1,2,3,4),
    #                                limits=c(1,4)))}+
    #       {if(max(map_df_temp()$No.TCC)>4)list(
    #         scale_color_continuous(low = "thistle3", high = "darkred", 
    #                                guide="colorbar", name="Number of TCCs",
    #                                breaks=pretty_breaks()))}+
    #       new_scale_color()+
    #       scale_shape_manual(values=c("SHQ"=3,
    #                                   "UNMO"=24,
    #                                   "UNPOL"=23),
    #                          labels=c("SHQ"="Sector HQ", 
    #                                   "UNMO"="Military Observers", 
    #                                   "UNPOL"="UN Police"),
    #                          name="Non-combat functions")+
    #       scale_color_manual(values=c("SHQ"="orange",
    #                                   "UNMO"="darkblue",
    #                                   "UNPOL"="darkgreen"),
    #                          labels=c("SHQ"="Sector HQ", 
    #                                   "UNMO"="Military Observers", 
    #                                   "UNPOL"="UN Police"),
    #                          name="Non-combat functions")
    #   }
    #  }
    
    if(input$MHQ_map){
      p <- p +  geom_point(data=map_df_temp() %>% filter(HQ==3) %>% slice(1),
                           aes(x=Longitude, y=Latitude, shape="HQ"),
                           shape=4, color="red", size=6)+
        geom_label_repel(data=map_df_temp() %>% filter(HQ==3),
                         aes(x=Longitude, y=Latitude, label=paste0("Mission HQ: ",Location)
                         ),
                         box.padding = 2,
                         size = 3,
                         fill = alpha(c("white"),0.7))
    }
    
    if(input$SHQ_map){
      if(length(SHQ_df_temp()$Location)>1){
        p <- p +  geom_point(data=map_df_temp() %>% filter(HQ==2), 
                             aes(x=Longitude, y= Latitude, shape="SHQ", color="SHQ"), size=5)}
      else{
        p <- p + labs(subtitle="Sector HQs not available. Please deselect the option.")}  
    }
    
    if(input$MO_map){
      if(length(UNMO_df_temp()$Location)>1){
        p <- p +  geom_point(data=map_df_temp() %>% filter(UNMO.dummy==1), 
                             aes(x=Longitude, y= Latitude, shape="UNMO", color="UNMO"),
                             #color="darkblue", 
                             position=position_jitter(),
                             size=3)}
      else{
        p <- p + labs(subtitle="UNMO not found. Please deselect the option.")}  
    }
    
    if(input$UNPOL_map){
      if(length(UNPOL_df_temp()$Location)>1){
        p <- p +  geom_point(data=map_df_temp() %>% filter(UNPOL.dummy==1), 
                             aes(x=Longitude, y= Latitude, shape="UNPOL", color="UNPOL"),
                             position=position_jitter(),
                             size=4)}
      else{
        p <- p + labs(subtitle="UNPOL not found. Please deselect the option.")}
    }
    
    p <- p +
      theme(plot.subtitle = element_text(color="red"),
            plot.title=element_text(face="bold", hjust=0),
            #      plot.caption.position = "plot",
            plot.caption = element_text(hjust=1),
            legend.direction = "horizontal", 
            legend.position = "bottom", 
            legend.box = "vertical")
    
    print(p)
    
  }, height=600) 
  
  ####map_df_detail####
  
  typecheck_df <- reactive({
    map_df_temp() %>% tibble::rowid_to_column("ID") %>% 
      select(ID, Location, No.troops, No.TCC, RPF:UAV, Other.Type, -RPF_No,
             -Inf_No, -FPU_No, -RES_No, -FP_No) %>%
      mutate_at(vars(RPF:Other.Type), as.numeric) %>% 
      rowwise(ID) %>% 
      mutate(typecheck_var=sum(c_across(RPF:Other.Type))) %>% 
      filter(typecheck_var >0)
  })
  
  static_map_details <- reactive({
    if(length(typecheck_df()>0)){
      map_df_temp() %>% tibble::rowid_to_column("ID") %>% 
        select(ID, Location, No.troops, No.TCC, RPF:UAV, Other.Type, -RPF_No,
               -Inf_No, -FPU_No, -RES_No, -FP_No) %>% 
        mutate(across(everything(), as.character)) %>% 
        pivot_longer(5:23, names_to="trooptypes", values_to="binary") %>% 
        filter(binary==1) %>% 
        mutate(trooptypes=ifelse(trooptypes=="Other.Type", "Others", trooptypes)) %>% 
        group_by(ID, Location, No.troops, No.TCC) %>% 
        summarize(Troop.Compo = str_c(trooptypes, collapse=", ")) %>% ungroup() %>% 
        select(-ID)}
    else {
      map_df_temp() %>% 
        select(Location, No.troops, No.TCC) %>% 
        mutate(Troop.type="Data on troop types not available for this location")
    }
  })
  
  output$map_df_details <- renderDataTable({
    DT::datatable(static_map_details(), 
                  rownames = FALSE)
    
  })
  
  ####animated maps####
  anim_sf <- reactive({
    req(input$anim_map)
    cclist3 %>% filter(Mission %in% input$anim_map)
  })
  
  anim_df <- reactive({
    req(input$anim_map)
    map_df %>% filter(Mission %in% input$anim_map) %>% arrange(timepoint)
  }) 
  
  
  
  output$animated <- renderImage({
    outfile <- tempfile(fileext= '.gif')
    
    anim_maplist <- pull(anim_sf(), a3)
    anim_mapshapefiles <- gadm_sf_loadCountries(c(paste(anim_maplist)), level=1)
    mission_name <- anim_df() %>% distinct(Mission)
    colourCount = max(anim_df()$No.TCC)
    getPalette = colorRampPalette(brewer.pal(9, "Set1"))
    
    anim_p <- ggplot() + geom_sf(data=anim_mapshapefiles$sf) + 
      theme_void() + 
      geom_point(data=anim_df(), aes(x=Longitude, y=Latitude, size=No.troops, 
                                     color=as.integer(No.TCC), group=timepoint),
                 shape=20, alpha = 0.5)+
      scale_size_binned(name="Size of deployment",range=c(2, 16))+
      {if(max(anim_df()$No.TCC)<=4)list(
        scale_color_continuous(low = "thistle3", high = "darkred",
                               guide="colorbar", name="No. of Troop-\nContributing Countries",
                               breaks=c(1,2,3,4),
                               limits=c(1,4)))
      } +
      {if(max(anim_df()$No.TCC)>4)list(
        scale_color_continuous(low = "thistle3", high = "darkred",
                               guide="colorbar", name="No. of Troop-\nContributing Countries",
                               breaks=pretty_breaks())
      )}+
      theme(plot.subtitle = element_text(color="red"),
            plot.title=element_text(face="bold", hjust=0),
            #      plot.caption.position = "plot",
            plot.caption = element_text(hjust=1),
            legend.direction = "vertical",
            legend.box="horizontal",
            legend.position = "right")+
      transition_states(states=anim_df()$timepoint)+
      labs(title=paste0(mission_name,": ", "{closest_state}"),
           caption="Sources: Geo-PKO v2.0\n Shapefiles from GADM.")+
      ease_aes('linear')
    
    anim_save("outfile.gif", animate(anim_p, fps = 4, width=650, height=500, res=120))
    
    list(src="outfile.gif",
         contentType='image/gif'
    )}, deleteFile= TRUE)
  
  
  # observeEvent(input$anim_map,{
  #   anim_temp <- map_df %>% filter(Mission %in% input$anim_map)
  #   
  #   updateSliderInput(session, 'anim_timepoint',
  #                     min = as.Date(min(anim_temp$slider_time), "%Y-%B"),
  #                     max = as.Date(max(anim_temp$slider_time), "%Y-%B"))
  #   
  # }) reserve for time input
  
  # map_df_temp <- reactive({
  #   req(input$mission_map)
  #   req(input$timepoint_map)
  #   map_df %>% filter(Mission %in% input$mission_map) %>%
  #     filter(timepoint %in% input$timepoint_map)
  #   
  # })
  
  ####lollipop####
  lollipop_df <- reactive({
    req(input$Lollipop_map)
    geopko %>% group_by(Mission, Location)%>% 
      summarize(start_date=min(Year), end_date=max(Year)) %>% 
      filter(Mission %in% input$Lollipop_map) %>% 
      mutate_at(vars(c(start_date, end_date)), as.numeric)
  })
  
  height_lollipop <-  reactive({
    if(nrow(lollipop_df())<15){400}
    else{NROW(lollipop_df())*25+300}
  })
  
  
  
  output$lollipop <- renderPlot({
    
    
    lolli <-   ggplot(lollipop_df()) +
      geom_segment(aes(x=start_date, xend=end_date, 
                       y=fct_reorder(Location, start_date), 
                       yend=fct_reorder(Location, start_date)), color="grey") +
      geom_point(aes(x=end_date, y=Location), 
                 colour=rgb(0.9,0.3,0.1,0.9), size=3.5 ) +
      geom_point(aes(x=start_date, y=Location), 
                 colour=rgb(1.0,0.6,0.1,0.7), size=3) +
      scale_x_continuous(breaks = 
                           seq(1993,2020,1))+
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none",
            axis.text.x = element_text(angle=45, hjust=1),
            axis.ticks.length.x= unit(0.1, "cm"),
            panel.grid.minor.x = element_blank(),
            panel.spacing.x = unit(1,"lines")
            
      ) +
      xlab("Years") +
      ylab("")+ #title already mentions locations, so no need for name
      labs(title=paste0(lollipop_df()$Mission), # to add (note from T but anyone can do) - , ": ", [mission's earliest year], " - ", [mission's latest year / "-" if ongoing]
           caption="Data: Geo-PKO v2.0")
    lolli
  }, height=height_lollipop)
  
}  



shinyApp(ui=ui, server=server)
