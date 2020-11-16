library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(geojsonio)
library(shinythemes)

nyboros <- geojsonio::geojson_read("json/Borough Boundaries.geojson", what = "sp") 
#prepare color mapping
nyboros$boro_name <- factor(nyboros$boro_name)
factpal <- colorFactor("Set1", nyboros$boro_name)
# read the model 
model <- read_rds("data/model.RDS")
df <- read_csv("data/pred_df.csv", col_types = cols(boro = col_factor(levels = c("BROOKLYN", 
                                                                             "BRONX", 
                                                                             "QUEENS", 
                                                                             "MANHATTAN",
                                                                             "STATEN ISLAND")),
                                                crime_type = col_factor(levels = c("violent",
                                                                                   "theft_property",
                                                                                   "other"))))
hol <<-read_csv("data/usholidays.csv") 

ui <- fluidPage(
    theme = shinytheme("superhero"),
    sidebarLayout(
    sidebarPanel(top = 10, left = 10,
                  dateInput("date","Date"),
                  selectInput("awmd","Windy?",c("yes"=1,"no"=0),selected = 0),
                  selectInput("prcp","Raining?",c("yes" =1, "no"=0),selected = 0),
                  selectInput("snow", "Snow?", c("yes"=1, "no"=0),selected = 0),
                  sliderInput("tmax", "Max Temp", min = -10, max = 115, value =65),
                  sliderInput("tmin", "Min Temp", min =-40, max= 115, value = 50),
                  sliderInput("wsf2","How strong are the Gusts? (mph)", max =70, min = 0, value =5),
                  selectInput("fog","Foggy?",c("heavy"=2,"light"=1,"no"=0),selected = 0),
                  selectInput("wt03", "Thunderstorms?", c("yes"=1,"no"=0),selected = 0),
                  selectInput("wt04", "Sleet?", c("yes"=1,"no"=0), selected = 0),
                  selectInput("wt06", "Frosty or Icy?", c("yes" =1, no ="0"), selected = 0),
                  selectInput("wt08", "Smokey or Hazey?", c("yes" =1, no ="0"), selected = 0)
    ),
    mainPanel(
              leafletOutput("map"),
              tableOutput("pred")
))
)
server <- function(input, output, session) {
   
    # month <- 
    # weekday <- observe()
    #holiday <- observe(ifelse(input$date%in%lubridate::mdy(hol$Date), 1,0))

     output$pred <- renderTable({
             pred_df <<-df %>% 
                bind_cols(tibble(month = rep(lubridate::month(input$date),15),
                         awnd = as.double(input$awmd),
                         prcp = as.double(input$prcp),
                         snow = as.double(input$snow),
                         tmax = as.double(input$tmax),
                         tmin = as.double(input$tmin),
                         wsf2 = as.double(input$wsf2),
                         wt01 = as.double(ifelse(input$fog==2,1,0)),
                         wt02 = as.double(ifelse(input$fog==1,1,0)),
                         wt03 = as.double(input$wt03),
                         wt04 = as.double(input$wt04),
                         wt06 = as.double(input$wt06),
                         wt08 = as.double(input$wt08),
                         holiday = ifelse(input$date%in%lubridate::mdy(hol$Date), 1,0),
                         weekday = lubridate::wday(input$date))) %>% 
                 group_by(boro,crime_type) %>% 
                nest()
            
              preds <<- df %>% 
                 mutate(pred = map2(model$model,pred_df$data,predict)) %>% 
                 unnest(pred)
                             
                
                        
            
            
         }
     )
    

    output$map <- renderLeaflet({
        
        pred_geo <-tibble(boro = c("BROOKLYN", 
                            "BRONX", 
                            "QUEENS", 
                            "MANHATTAN",
                            "STATEN ISLAND"),
                          lat = c(40.6500,40.8448,40.7420,40.7831,40.5790),
                          long = c(-73.9499,-73.8648,-73.7694,-73.9712,-74.1515)) %>% 
            left_join(preds)
        
        flag1 <- makeAwesomeIcon(icon = "flag", markerColor = "blue",
                                          iconColor = "black", library = "ion",
                                          squareMarker =  TRUE)
        flag2 <- makeAwesomeIcon(icon = "flag", markerColor = "red", library = "ion",
                                   iconColor = "black")
        flag3 <- makeAwesomeIcon(icon = "flag", markerColor = "green",
                                    library = "ion")
        

        leaflet() %>% 
            addPolygons(data = nyboros, weight = 2, fillColor = ~factpal(boro_name), group = "Boros") %>% 
            setView(-74,40.7,zoom=10) %>% 
            addTiles(group = "Default") %>% 
            addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>% 
            addAwesomeMarkers(data = pred_geo %>% filter(crime_type=="violent") ,~long, ~lat,popup = ~as.character(crime_type), label =  ~as.character(round(pred,1)), icon=flag1,group = "violent") %>%
            addAwesomeMarkers(data = pred_geo %>% filter(crime_type == "theft_property") ,~long+0.015, ~lat+0.015,popup = ~as.character(crime_type),  label = ~as.character(round(pred,1)), icon=flag2,group = "theft_property") %>% 
            addAwesomeMarkers(data = pred_geo %>% filter(crime_type=="other") ,~long-0.015, ~lat-0.015,popup = ~as.character(crime_type), label = ~as.character(round(pred,1)), icon=flag3,group = "other") %>% 
            addLayersControl(
                baseGroups = c("Default","Dark"),
                overlayGroups = c("violent", "theft", "other"),
                options = layersControlOptions(collapsed = T))
    })
    
}

shinyApp(ui, server)