library(shiny)
library(shinydashboard)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
library(leaflet)
library(htmltools)

pedestrian_data <- read_csv("https://raw.githubusercontent.com/psumank/DATA698/master/Data/pedestrian-incidents-fulldata-latest.csv")
pedestrian_hotspots <- read_csv("https://raw.githubusercontent.com/psumank/DATA698/master/Data/pedestrian-hotspots-fulldata-latest.csv")

threeveh_data2 <- read_csv("https://raw.githubusercontent.com/psumank/DATA698/master/Data/threevehicle-incidents-fulldata-latest.csv")
threeveh_hotspots2 <- read_csv("https://raw.githubusercontent.com/psumank/DATA698/master/Data/threevehicle-hotspots-fulldata-latest.csv")

server <- function(input, output, session) {
  
 
  reactive_pedincidents_data <- reactive(

    if (input$incidentcat == 'Pedestrian Incidents')
    {
      if (input$vz == 'After VisionZero') {
         if (input$borough == 'ALL') {
            if (input$vztype == 'ANY' && input$vzexists) {
              filter(pedestrian_data, pedestrian_data['NBR_VZ_INITIATIVES'] > 0, pedestrian_data['VZ'] == 1)
            }
            else if (input$vztype == 'ANY' && !input$vzexists) {
              filter(pedestrian_data, pedestrian_data['NBR_VZ_INITIATIVES'] == 0, pedestrian_data['VZ'] == 1)
            }
            else {
              filter(pedestrian_data, pedestrian_data[[input$vztype]] == input$vzexists, pedestrian_data['VZ'] == 1)
            }
            
          } else {
            if (input$vztype == 'ANY' && input$vzexists) {
              filter(pedestrian_data, BOROUGH %in% input$borough, pedestrian_data['NBR_VZ_INITIATIVES'] > 0, pedestrian_data['VZ'] == 1)
            }
            else if (input$vztype == 'ANY' && !input$vzexists) {
              filter(pedestrian_data, BOROUGH %in% input$borough, pedestrian_data['NBR_VZ_INITIATIVES'] == 0, pedestrian_data['VZ'] == 1)
            } else {
              filter(pedestrian_data, BOROUGH %in% input$borough, pedestrian_data[[input$vztype]] == input$vzexists, pedestrian_data['VZ'] == 1)
            }
          }
      }
      else if (input$vz == 'Before VisionZero') { 
        if (input$borough == 'ALL') {
            filter(pedestrian_data, pedestrian_data['VZ'] == 0)
        } else {
            filter(pedestrian_data, BOROUGH %in% input$borough, pedestrian_data['VZ'] == 0)
        }
      }
      else {
        if (input$borough == 'ALL') {
          if (input$vztype == 'ANY' && input$vzexists) {
            filter(pedestrian_data, pedestrian_data['NBR_VZ_INITIATIVES'] > 0)
          }
          else if (input$vztype == 'ANY' && !input$vzexists) {
            filter(pedestrian_data, pedestrian_data['NBR_VZ_INITIATIVES'] == 0)
          }
          else {
            filter(pedestrian_data, pedestrian_data[[input$vztype]] == input$vzexists)
          }
          
        } else {
          if (input$vztype == 'ANY' && input$vzexists) {
            filter(pedestrian_data, BOROUGH %in% input$borough, pedestrian_data['NBR_VZ_INITIATIVES'] > 0)
          }
          else if (input$vztype == 'ANY' && !input$vzexists) {
            filter(pedestrian_data, BOROUGH %in% input$borough, pedestrian_data['NBR_VZ_INITIATIVES'] == 0)
          } else {
            filter(pedestrian_data, BOROUGH %in% input$borough, pedestrian_data[[input$vztype]] == input$vzexists)
          }
        }
      }
    }
    else  #non pedestrain
    {
      if (input$vz == 'After VisionZero') {
        if (input$borough == 'ALL') {
          if (input$vztype == 'ANY' && input$vzexists) {
            filter(threeveh_data2, threeveh_data2['NBR_VZ_INITIATIVES'] > 0, threeveh_data2['VZ'] == 1)
          }
          else if (input$vztype == 'ANY' && !input$vzexists) {
            filter(threeveh_data2, threeveh_data2['NBR_VZ_INITIATIVES'] == 0, threeveh_data2['VZ'] == 1)
          }
          else {
            filter(threeveh_data2, threeveh_data2[[input$vztype]] == input$vzexists, threeveh_data2['VZ'] == 1)
          }
          
        } else {
          if (input$vztype == 'ANY' && input$vzexists) {
            filter(threeveh_data2, BOROUGH %in% input$borough, threeveh_data2['NBR_VZ_INITIATIVES'] > 0, threeveh_data2['VZ'] == 1)
          }
          else if (input$vztype == 'ANY' && !input$vzexists) {
            filter(threeveh_data2, BOROUGH %in% input$borough, threeveh_data2['NBR_VZ_INITIATIVES'] == 0, threeveh_data2['VZ'] == 1)
          } else {
            filter(threeveh_data2, BOROUGH %in% input$borough, threeveh_data2[[input$vztype]] == input$vzexists, threeveh_data2['VZ'] == 1)
          }
        }
      }
      else if (input$vz == 'Before VisionZero') { 
        if (input$borough == 'ALL') {
          filter(threeveh_data2, threeveh_data2['VZ'] == 0)
        } else {
          filter(threeveh_data2, BOROUGH %in% input$borough, threeveh_data2['VZ'] == 0)
        }
      }
      else {
        if (input$borough == 'ALL') {
          if (input$vztype == 'ANY' && input$vzexists) {
            filter(threeveh_data2, threeveh_data2['NBR_VZ_INITIATIVES'] > 0)
          }
          else if (input$vztype == 'ANY' && !input$vzexists) {
            filter(threeveh_data2, threeveh_data2['NBR_VZ_INITIATIVES'] == 0)
          }
          else {
            filter(threeveh_data2, threeveh_data2[[input$vztype]] == input$vzexists)
          }
          
        } else {
          if (input$vztype == 'ANY' && input$vzexists) {
            filter(threeveh_data2, BOROUGH %in% input$borough, threeveh_data2['NBR_VZ_INITIATIVES'] > 0)
          }
          else if (input$vztype == 'ANY' && !input$vzexists) {
            filter(threeveh_data2, BOROUGH %in% input$borough, threeveh_data2['NBR_VZ_INITIATIVES'] == 0)
          } else {
            filter(threeveh_data2, BOROUGH %in% input$borough, threeveh_data2[[input$vztype]] == input$vzexists)
          }
        }
      }
      
    }
  )
  
  reactive_pedestrian_hotspots <- reactive(
    
    if (input$incidentcat == 'Pedestrian Incidents') {
      if (input$vz == 'After VisionZero') {
        if (input$borough == 'ALL') {
          if (input$vztype == 'ANY' && input$vzexists) {
            filter(pedestrian_hotspots, pedestrian_hotspots['VZ'] == 1, pedestrian_hotspots['NBR_VZ_INITIATIVES'] > 0, pedestrian_hotspots['EPSILON'] == input$epsilon, pedestrian_hotspots['SAMPLES'] == input$samples)
          }else if (input$vztype == 'ANY' && !input$vzexists) {
            filter(pedestrian_hotspots, pedestrian_hotspots['VZ'] == 1, pedestrian_hotspots['NBR_VZ_INITIATIVES'] == 0, pedestrian_hotspots['EPSILON'] == input$epsilon, pedestrian_hotspots['SAMPLES'] == input$samples)
          } else{
            filter(pedestrian_hotspots, pedestrian_hotspots['VZ'] == 1, pedestrian_hotspots[[input$vztype]] == input$vzexists, pedestrian_hotspots['EPSILON'] == input$epsilon, pedestrian_hotspots['SAMPLES'] == input$samples)
          }
        } else {
          if (input$vztype == 'ANY' && input$vzexists) {
            filter(pedestrian_hotspots, BOROUGH %in% input$borough, pedestrian_hotspots['VZ'] == 1, pedestrian_hotspots['NBR_VZ_INITIATIVES'] > 0, pedestrian_hotspots['EPSILON'] == input$epsilon, pedestrian_hotspots['SAMPLES'] == input$samples)
          } else if (input$vztype == 'ANY' && !input$vzexists) {
            filter(pedestrian_hotspots, BOROUGH %in% input$borough, pedestrian_hotspots['VZ'] == 1, pedestrian_hotspots['NBR_VZ_INITIATIVES'] == 0, pedestrian_hotspots['EPSILON'] == input$epsilon, pedestrian_hotspots['SAMPLES'] == input$samples)
          } else {
            filter(pedestrian_hotspots, BOROUGH %in% input$borough, pedestrian_hotspots['VZ'] == 1, pedestrian_hotspots[[input$vztype]] == input$vzexists, pedestrian_hotspots['EPSILON'] == input$epsilon, pedestrian_hotspots['SAMPLES'] == input$samples)
          }
        }
      }
      else if (input$vz == 'Before VisionZero'){
        if (input$borough == 'ALL') {
          if (input$vztype == 'ANY' && input$vzexists) {
            filter(pedestrian_hotspots, pedestrian_hotspots['VZ'] == 0, pedestrian_hotspots['NBR_VZ_INITIATIVES'] > 0, pedestrian_hotspots['EPSILON'] == input$epsilon, pedestrian_hotspots['SAMPLES'] == input$samples)
          }else if (input$vztype == 'ANY' && !input$vzexists) {
            filter(pedestrian_hotspots, pedestrian_hotspots['VZ'] == 0, pedestrian_hotspots['NBR_VZ_INITIATIVES'] == 0, pedestrian_hotspots['EPSILON'] == input$epsilon, pedestrian_hotspots['SAMPLES'] == input$samples)
          } else{
            filter(pedestrian_hotspots, pedestrian_hotspots['VZ'] == 0, pedestrian_hotspots[[input$vztype]] == input$vzexists, pedestrian_hotspots['EPSILON'] == input$epsilon, pedestrian_hotspots['SAMPLES'] == input$samples)
          }
        } else {
          if (input$vztype == 'ANY' && input$vzexists) {
            filter(pedestrian_hotspots, BOROUGH %in% input$borough, pedestrian_hotspots['VZ'] == 0, pedestrian_hotspots['NBR_VZ_INITIATIVES'] > 0, pedestrian_hotspots['EPSILON'] == input$epsilon, pedestrian_hotspots['SAMPLES'] == input$samples)
          } else if (input$vztype == 'ANY' && !input$vzexists) {
            filter(pedestrian_hotspots, BOROUGH %in% input$borough, pedestrian_hotspots['VZ'] == 0, pedestrian_hotspots['NBR_VZ_INITIATIVES'] == 0, pedestrian_hotspots['EPSILON'] == input$epsilon, pedestrian_hotspots['SAMPLES'] == input$samples)
          } else {
            filter(pedestrian_hotspots, BOROUGH %in% input$borough, pedestrian_hotspots['VZ'] == 0, pedestrian_hotspots[[input$vztype]] == input$vzexists, pedestrian_hotspots['EPSILON'] == input$epsilon, pedestrian_hotspots['SAMPLES'] == input$samples)
          }
        }
      }
      else {
        if (input$borough == 'ALL') {
          if (input$vztype == 'ANY' && input$vzexists) {
            filter(pedestrian_hotspots, pedestrian_hotspots['NBR_VZ_INITIATIVES'] > 0, pedestrian_hotspots['EPSILON'] == input$epsilon, pedestrian_hotspots['SAMPLES'] == input$samples)
          }else if (input$vztype == 'ANY' && !input$vzexists) {
            filter(pedestrian_hotspots, pedestrian_hotspots['NBR_VZ_INITIATIVES'] == 0, pedestrian_hotspots['EPSILON'] == input$epsilon, pedestrian_hotspots['SAMPLES'] == input$samples)
          } else{
            filter(pedestrian_hotspots, pedestrian_hotspots[[input$vztype]] == input$vzexists, pedestrian_hotspots['EPSILON'] == input$epsilon, pedestrian_hotspots['SAMPLES'] == input$samples)
          }
        } else {
          if (input$vztype == 'ANY' && input$vzexists) {
            filter(pedestrian_hotspots, BOROUGH %in% input$borough, pedestrian_hotspots['NBR_VZ_INITIATIVES'] > 0, pedestrian_hotspots['EPSILON'] == input$epsilon, pedestrian_hotspots['SAMPLES'] == input$samples)
          } else if (input$vztype == 'ANY' && !input$vzexists) {
            filter(pedestrian_hotspots, BOROUGH %in% input$borough, pedestrian_hotspots['NBR_VZ_INITIATIVES'] == 0, pedestrian_hotspots['EPSILON'] == input$epsilon, pedestrian_hotspots['SAMPLES'] == input$samples)
          } else {
            filter(pedestrian_hotspots, BOROUGH %in% input$borough, pedestrian_hotspots[[input$vztype]] == input$vzexists, pedestrian_hotspots['EPSILON'] == input$epsilon, pedestrian_hotspots['SAMPLES'] == input$samples)
          }
        }
      }
    }
    else
    {
      if (input$vz == 'After VisionZero') {
        if (input$borough == 'ALL') {
          if (input$vztype == 'ANY' && input$vzexists) {
            filter(threeveh_hotspots2, threeveh_hotspots2['VZ'] == 1, threeveh_hotspots2['NBR_VZ_INITIATIVES'] > 0, threeveh_hotspots2['EPSILON'] == input$epsilon, threeveh_hotspots2['SAMPLES'] == input$samples)
          }else if (input$vztype == 'ANY' && !input$vzexists) {
            filter(threeveh_hotspots2, threeveh_hotspots2['VZ'] == 1, threeveh_hotspots2['NBR_VZ_INITIATIVES'] == 0, threeveh_hotspots2['EPSILON'] == input$epsilon, threeveh_hotspots2['SAMPLES'] == input$samples)
          } else{
            filter(threeveh_hotspots2, threeveh_hotspots2['VZ'] == 1, threeveh_hotspots2[[input$vztype]] == input$vzexists, threeveh_hotspots2['EPSILON'] == input$epsilon, threeveh_hotspots2['SAMPLES'] == input$samples)
          }
        } else {
          if (input$vztype == 'ANY' && input$vzexists) {
            filter(threeveh_hotspots2, BOROUGH %in% input$borough, threeveh_hotspots2['VZ'] == 1, threeveh_hotspots2['NBR_VZ_INITIATIVES'] > 0, threeveh_hotspots2['EPSILON'] == input$epsilon, threeveh_hotspots2['SAMPLES'] == input$samples)
          } else if (input$vztype == 'ANY' && !input$vzexists) {
            filter(threeveh_hotspots2, BOROUGH %in% input$borough, threeveh_hotspots2['VZ'] == 1, threeveh_hotspots2['NBR_VZ_INITIATIVES'] == 0, threeveh_hotspots2['EPSILON'] == input$epsilon, threeveh_hotspots2['SAMPLES'] == input$samples)
          } else {
            filter(threeveh_hotspots2, BOROUGH %in% input$borough, threeveh_hotspots2['VZ'] == 1, threeveh_hotspots2[[input$vztype]] == input$vzexists, threeveh_hotspots2['EPSILON'] == input$epsilon, threeveh_hotspots2['SAMPLES'] == input$samples)
          }
        }
      }
      else if (input$vz == 'Before VisionZero'){
        if (input$borough == 'ALL') {
          if (input$vztype == 'ANY' && input$vzexists) {
            filter(threeveh_hotspots2, threeveh_hotspots2['VZ'] == 0, threeveh_hotspots2['NBR_VZ_INITIATIVES'] > 0, threeveh_hotspots2['EPSILON'] == input$epsilon, threeveh_hotspots2['SAMPLES'] == input$samples)
          }else if (input$vztype == 'ANY' && !input$vzexists) {
            filter(threeveh_hotspots2, threeveh_hotspots2['VZ'] == 0, threeveh_hotspots2['NBR_VZ_INITIATIVES'] == 0, threeveh_hotspots2['EPSILON'] == input$epsilon, threeveh_hotspots2['SAMPLES'] == input$samples)
          } else{
            filter(threeveh_hotspots2, threeveh_hotspots2['VZ'] == 0, threeveh_hotspots2[[input$vztype]] == input$vzexists, threeveh_hotspots2['EPSILON'] == input$epsilon, threeveh_hotspots2['SAMPLES'] == input$samples)
          }
        } else {
          if (input$vztype == 'ANY' && input$vzexists) {
            filter(threeveh_hotspots2, BOROUGH %in% input$borough, threeveh_hotspots2['VZ'] == 0, threeveh_hotspots2['NBR_VZ_INITIATIVES'] > 0, threeveh_hotspots2['EPSILON'] == input$epsilon, threeveh_hotspots2['SAMPLES'] == input$samples)
          } else if (input$vztype == 'ANY' && !input$vzexists) {
            filter(threeveh_hotspots2, BOROUGH %in% input$borough, threeveh_hotspots2['VZ'] == 0, threeveh_hotspots2['NBR_VZ_INITIATIVES'] == 0, threeveh_hotspots2['EPSILON'] == input$epsilon, threeveh_hotspots2['SAMPLES'] == input$samples)
          } else {
            filter(threeveh_hotspots2, BOROUGH %in% input$borough, threeveh_hotspots2['VZ'] == 0, threeveh_hotspots2[[input$vztype]] == input$vzexists, threeveh_hotspots2['EPSILON'] == input$epsilon, threeveh_hotspots2['SAMPLES'] == input$samples)
          }
        }
      }
      else {
        if (input$borough == 'ALL') {
          if (input$vztype == 'ANY' && input$vzexists) {
            filter(threeveh_hotspots2, threeveh_hotspots2['NBR_VZ_INITIATIVES'] > 0, threeveh_hotspots2['EPSILON'] == input$epsilon, threeveh_hotspots2['SAMPLES'] == input$samples)
          }else if (input$vztype == 'ANY' && !input$vzexists) {
            filter(threeveh_hotspots2, threeveh_hotspots2['NBR_VZ_INITIATIVES'] == 0, threeveh_hotspots2['EPSILON'] == input$epsilon, threeveh_hotspots2['SAMPLES'] == input$samples)
          } else{
            filter(threeveh_hotspots2, threeveh_hotspots2[[input$vztype]] == input$vzexists, threeveh_hotspots2['EPSILON'] == input$epsilon, threeveh_hotspots2['SAMPLES'] == input$samples)
          }
        } else {
          if (input$vztype == 'ANY' && input$vzexists) {
            filter(threeveh_hotspots2, BOROUGH %in% input$borough, threeveh_hotspots2['NBR_VZ_INITIATIVES'] > 0, threeveh_hotspots2['EPSILON'] == input$epsilon, threeveh_hotspots2['SAMPLES'] == input$samples)
          } else if (input$vztype == 'ANY' && !input$vzexists) {
            filter(threeveh_hotspots2, BOROUGH %in% input$borough, threeveh_hotspots2['NBR_VZ_INITIATIVES'] == 0, threeveh_hotspots2['EPSILON'] == input$epsilon, threeveh_hotspots2['SAMPLES'] == input$samples)
          } else {
            filter(threeveh_hotspots2, BOROUGH %in% input$borough, threeveh_hotspots2[[input$vztype]] == input$vzexists, threeveh_hotspots2['EPSILON'] == input$epsilon, threeveh_hotspots2['SAMPLES'] == input$samples)
          }
        }
      }
      
    }

  )
  
  #maptext
  output$maptext <- renderUI({ 
    if (nrow(reactive_pedestrian_hotspots()) > 0) {
      HTML(paste("Clustered the total incident records ", formatC(nrow(reactive_pedincidents_data()), format="d", big.mark=",") , " into ", nrow(reactive_pedestrian_hotspots())  , " hotspots using DBSCAN:<br>"))
    }else {
      paste("NO HOT SPOTS FOUND !")
    }
  })
  
  #vzText
  output$vzText <- renderUI({ 
   
    if (input$vz != 'Before VisionZero')
    {
      if (input$vztype == 'SLOWZONE') {
        HTML("<p style=margin:0.8em;>The Arterial Slow Zone program uses a combination of a lower speed limit, signal timing changes, distinctive signs and 
             increased enforcement to improve safety on some of New York City's most high-crash corridors</p>")
      } else if (input$vztype == 'SPEED_HUMP') {
        HTML("<p style=margin:0.8em;>Speed Humps are a raised area of a roadway designed to reduce vehicle speeds</p>")
      } else if (input$vztype == 'SIGNAL_TIMING') {
        HTML("<p style=margin:0.8em;>Priority Corridors where the signal progression has been changed to match the 25 MPH speed limit.</p>")
      } else if (input$vztype == 'BIKE_PRIORITY') {
        HTML("<p style=margin:0.8em;>Priority Bicycle Districts are neighborhoods with comparatively high numbers of cyclist KSI and
              few dedicated bicycle facilities. These districts, seven in Brooklyn and three in Queens, represent 14% of the City's bicycle lane network and 23% of cyclist KSI. NYC DOT identified these areas
              in the 2017 report Safer Cycling: Bicycle Ridership and Safety in New York City. The agency has prioritized these areas for bicycle network expansion.</p>")
      } else if (input$vztype == 'ENHANCED_CROSSING') {
        HTML("<p style=margin:0.8em;>Enhanced Crossings are marked high-visibility crosswalks on calm streets with low vehicle
            volumes and a strong pedestrian desire to cross. Standard DOT toolbox treatments are used (ADA pedestrian ramps, pedestrian warning signs and high-visibility crosswalk markings) to
             improve the mobility and accessibility of pedestrians.</p>")
      } else if (input$vztype == 'LEAD_PEDESTRIAN_INTERVAL') {
        HTML("<p style=margin:0.8em;>Intersections where DOT installs signals that show a walk sign for pedestrians before showing a
            green light to vehicle traffic. The goal of these signals is to improve street safety by giving pedestrians a chance to establish their presence in the crosswalk before vehicles make turns
            across that crosswalk.</p>")
      } else if (input$vztype == 'LEFT_TURN_TRAFFIC_CALMING') {
        HTML("<p style=margin:0.8em;>Intersections where DOT installs traffic calming measures that guide drivers to turn left at a safer
        speed and angle, as well as increase visibility for pedestrians in the crosswalk.</p>")
      } else if (input$vztype == 'NEIGHBORHOOD_SLOW_ZONE') {
        HTML("<p style=margin:0.8em;>The Neighborhood Slow Zone program is an application based program which takes a neighborhood area and reduces the speed limit to 20 mph. Areas are chosen based on crashes,
             presence of schools and other neighborhood amenities, and community support. The treatments include a mixture of markings, signage, and speed humps.</p>")
      } else if (input$vztype == 'SAFE_STREETS_FOR_SENIORS') {
        HTML("<p style=margin:0.8em;>The Safe Streets for Seniors program is an initiative aimed at increasing safety for older New Yorkers. Based on factors such as senior population density, injury crashes, and senior trip
             generators, DOT has selected and studied Senior Pedestrian Focus Areas. Within these areas, DOT evaluates potential safety improvements and also conducts educational outreach to senior
             centers.</p>")
      } else if (input$vztype == 'SIP_INTERSECTIONS' || input$vztype == 'SAFE_CORRIDORS') {
        HTML("<p style=margin:0.8em;>Safety-oriented engineering improvements that use multiple treatments (signals, markings,concrete etc) on both corridors and intersections. Improvements are generally aimed at better
             organizing traffic, improving travel times, creating shorter, safer pedestrian crossings, and safe routes for bicycle travel.</p>")
      } else if (input$vztype == 'PRIORITY_CORRIDORS') {
        HTML("<p style=margin:0.8em;>All corridors in each borough were ranked on a pedestrian KSI (killed and severely injured) per mile basis. Corridors were selected from the top of this list until the cumulative number of KSI
              reached half of the borough's total. Developed as part of the Borough Pedestrian Safety Action Plans.</p>")
      } else if (input$vztype == 'PRIORITY_INTERSECTIONS' || input$vztype == 'PRIORITY_ZONES') {
        HTML("<p style=margin:0.8em;>The intersections, zones with the highest number of pedestrian KSI (killed and severely injured) that cumulatively account for 15% of the borough's total pedestrian KSI. Developed as part of the
             Borough Pedestrian Safety Action Plans.</p>")
      } else if (input$vztype == 'ANY') {
        HTML("<p style=margin:0.8em;>Any of the Vision Zero initiative.</p>")
      }
    }

  })
  
  # Create the map
  output$map <- renderLeaflet({
    
    if (nrow(reactive_pedestrian_hotspots())  == 0) {
      return(NULL)
    }
    
    leaflet(reactive_pedestrian_hotspots()) %>% addTiles() %>%
      setView(lng = -74.06, lat = 40.73, zoom = 11)  %>%
        addMarkers(lat= ~LATITUDE,lng=~LONGITUDE, label = ~htmlEscape(paste(ON.STREET.NAME,'-',CROSS.STREET.NAME,'-',OFF.STREET.NAME,'-', BOROUGH)))
  }) 
  
  output$table <- DT::renderDataTable({
    DT::datatable(data = reactive_pedincidents_data(), 
                  options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$hsdata <- DT::renderDataTable({
    DT::datatable(data = reactive_pedestrian_hotspots(), 
                  options = list(pageLength = 10, scrollX = TRUE))
  })
}


body <- dashboardBody(
  
  tabsetPanel(type = "tabs",
              
    tabPanel('Map', 
             h2(uiOutput(outputId = "maptext")),
             p(uiOutput(outputId = "vzText")),
             leafletOutput("map",width="100%", height="600px")),
    
    tabPanel('Incident Data', 
             DT::dataTableOutput(outputId = "table")),
    
    tabPanel('Hot Spot Data', 
             DT::dataTableOutput(outputId = "hsdata"))
    
  )
)

sidebar <- dashboardSidebar(
  width = 300,

  selectInput(inputId = "incidentcat", label = "Incident Category", choices = c("Pedestrian Incidents", "Three vehicle incidents"), selected = "Pedestrian Incidents"),
  
  selectInput(inputId = "vz", label = "Incident Data Timeline", choices = c("ALL", "Before VisionZero", "After VisionZero"), selected = "ALL"),
    
  selectInput(inputId = "borough", label = "Borough", choices = c("QUEENS", "BROOKLYN", "MANHATTAN", "BRONX", "STATEN ISLAND", "ALL"), selected = "ALL"),
  
  conditionalPanel (
    
    condition = "input.vz == 'After VisionZero' || input.vz == 'ALL'",
  
    selectInput(inputId = "vztype", label = "Vision Zero Initiative", choices = c("ANY", "SLOWZONE", "SPEED_HUMP", "SIGNAL_TIMING", "BIKE_PRIORITY", "ENHANCED_CROSSING", "LEAD_PEDESTRIAN_INTERVAL", "LEFT_TURN_TRAFFIC_CALMING", "NEIGHBORHOOD_SLOW_ZONE", "SAFE_STREETS_FOR_SENIORS", "SIP_INTERSECTIONS", "SAFE_CORRIDORS", "PRIORITY_CORRIDORS", "PRIORITY_INTERSECTIONS", "PRIORITY_ZONES"), selected = "ANY"),
  
    checkboxInput(inputId = "vzexists",
                label = "Initiative in place ?",
                value = TRUE)
  ),

  # Add a slider for epsilon
  sliderInput(inputId="epsilon", label="epsilon", min=0.1, max=1.0, value=0.5, step = 0.1),
  
  # Add a slider for min_samples
  sliderInput(inputId="samples", label="minPoints", min=10, max=100, value=50, step = 10)
  
)

header <- dashboardHeader(
  title = "Traffic Incidents - Hotspot Viewer",
  titleWidth = 350
)

ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body
)

shinyApp(ui, server)