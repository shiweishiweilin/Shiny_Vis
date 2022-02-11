### Shiny Sample
### by Shiwei Lin 02/10/2022


library(sp)
library(rgdal)
library(data.table)
library(dygraphs)
library(plotly)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(xts) 
library(leaflet)
library(dygraphs)
library(plotly)
library(viridis)
library(vroom)
library(openxlsx)
library(data.table)


## sample patient data stored in local
getwd()
setwd("C:\\Users\\lamsw\\Desktop\\COVID")
df <- vroom("df.csv")
df <- df[, -1]
head(df)

## 11 variables of interest for the sample patient demographic
## Id, Ptid, Pt Group, Gender, Birth Yr, Race, Ethnicity, Region, Division, Deceased Indicator, Date Of Death

deceased <- filter(df, df$`Deceased Indicator` == 1)
Date <- as.Date(paste0(as.character(deceased$`Date Of Death`), '01'), format='%Y%m%d')
deceased <- cbind(deceased, Date)
head(deceased)

## map data preparation
data1 <- fread("deceased.csv")
data2 <- read.xlsx("census.xlsx")

map_data <- rgdal::readOGR(dsn ="./states_21basic/states_21basic/states.shp")
tem1 <- merge(data2,data1[,.(num1 =.N),.( Region)],by = "Region",all.x = T)
tem2 <- merge(tem1,data1[,.(num2 =.N),.( Division)],by.y="Division",by.x = "Division.DB",all.x = T)
usa_data <- sp::merge(map_data,tem2,by.x ="STATE_NAME",by.y = "State",all.x = T )


## Shiny
ui <- dashboardPage(
  
  dashboardHeader(title = "Patient Demographic"),
  
  dashboardSidebar(disable = FALSE, width = 150),
  
  dashboardBody(
    
    ## Descriptive Statistics
    fluidRow(
      valueBox(prettyNum(nrow(df), big.mark = ","), "Total number of patients", width = 3, icon = icon("hospital-user")),
      valueBox(prettyNum(nrow(deceased), big.mark = ","), "Total number of deaths", width = 3, icon = icon("file-medical")),
      valueBox(prettyNum(sum(df$Gender == 'Female'), big.mark = ","), "Female Patients", width = 3, icon = icon("female")),
      valueBox(prettyNum(sum(df$Gender == 'Male'), big.mark = ","), "Male Patients", width = 3, icon = icon("male")),
    ),
    
    ## Graphs
    fluidRow(
      box(title = "Deaths by Race", plotlyOutput('pie_race')),
      box(title = "Deaths by Ethnicity", plotlyOutput('pie_eth'))
    ),
    
    fluidRow(
      box(title = "Deaths after 2020", dygraphOutput("line_2020")),
      box(title = "Deaths by Gender", dygraphOutput("bar_gender"))
    ),
    
    fluidRow(
      box(title = "Deaths by Region", leafletOutput("map1")),
      box(title = "Deaths by Division", leafletOutput("map2"))
    )
    
  )
  
)


server <- function(input, output){
  
  ## plot1 pie chart death by race
  
  deceased.Race <- deceased %>% group_by(deceased$Race, deceased$`Deceased Indicator`) %>% summarise(number = n())
  colnames(deceased.Race) <- c("Race", "DeceasedIndicator", "DeathCount")
  
  output$pie_race <- renderPlotly(
    fig_race <- plot_ly(deceased.Race, labels = ~Race, values = ~DeathCount, type = 'pie', 
                   textposition = 'inside', textinfo = 'label+percent', hoverinfo = 'text',
                   text = ~paste('Number of Deaths:', DeathCount),
                   showlegend = FALSE)
  )
  
  ## plot2 pie chart death by ethnicity
  
  deceased.Ethnicity <- deceased %>% group_by(deceased$Ethnicity, deceased$`Deceased Indicator`) %>% summarise(number = n())
  colnames(deceased.Ethnicity) <- c("Ethnicity", "DeceasedIndicator", "DeathCount")
  
  output$pie_eth <- renderPlotly(
    fig_eth <- plot_ly(deceased.Ethnicity, labels = ~Ethnicity, values = ~DeathCount, type = 'pie', 
                   textposition = 'inside', textinfo = 'label+percent', hoverinfo = 'text',
                   text = ~paste('Number of Deaths:', DeathCount),
                   showlegend = FALSE)
  )
  
  
  ## plot3 line chart death after 2020
  
  deceased.Date <- deceased %>% group_by(deceased$Date, deceased$`Deceased Indicator`) %>% summarise(number = n())
  deceased.Date <- deceased.Date[,-2]
  colnames(deceased.Date) <- c("Date","Count")
  deceased.Date.after2020 <- subset(deceased.Date, Date >= "2020-01-01")
  don <- xts(x = deceased.Date.after2020$Count, order.by = deceased.Date.after2020$Date)
  
  output$line_2020 <- renderDygraph({
    dygraph(don,
            ylab = 'Number of Deaths',
            xlab = 'Date')%>% 
    dySeries("V1", label = "Count of Deaths") %>%
    dyLegend(show = "follow")
  })
  
  ## plot4 bar chart death by gender
  
  deceased.Gender <- deceased %>% group_by(deceased$Gender, deceased$Date, deceased$`Deceased Indicator`) %>% summarise(number = n())
  deceased.Gender <- deceased.Gender[, -3]
  colnames(deceased.Gender) <- c("Gender", "Date", "Count")
  deceased.Gender$Date <- as.Date(deceased.Gender$Date, format = "%Y-%m-%d")
  
  deceased.Gender.after2020 <- subset(deceased.Gender, Date>= "2020-01-01")
  genders <- split(deceased.Gender.after2020, deceased.Gender.after2020$Gender)
  Female <- as.data.frame(genders$Female)
  Male <- as.data.frame(genders$Male)
  
  female <- xts(x = Female$Count, order.by = Female$Date)
  male <- xts(x = Male$Count, order.by = Male$Date)
  d <- cbind(female, male)
  
  output$bar_gender <- renderDygraph({
    dygraph(d,
            ylab = 'Number of Deaths',
            xlab = 'Date')%>% 
      dyRangeSelector() %>%
      dyMultiColumn()%>% dyOptions(colors = c("orange","lightblue"))
  })
  
  ## plot5 map deaths by region
  output$map1 <- renderLeaflet({
    pal1 <- colorNumeric(
      palette = "viridis",
      domain = usa_data$num1) 
    
    leaflet( data = usa_data) %>%
      # addTiles() %>%
      #setView(-74,42,zoom=6) %>%
      addPolygons(
        
        stroke = TRUE,
        smoothFactor = 1,
        fillOpacity = 0.7,
        weight = 1,
        color = "black",
        popup = ~htmltools::htmlEscape(paste0(Region,":",num1)),
        label = ~paste0(Region,":",num1),
        fillColor = ~pal1(num1)
        
      ) %>% addLegend("topright", pal = pal1, values = ~num1,
                      title = "Deaths <br/> by Region",
                      labFormat = leaflet::labelFormat(prefix = ""),
                      opacity = 1) 
    
  })
    
    ## plot6 map deaths by division
    output$map2 <- renderLeaflet({
      
      pal2 <- colorNumeric(
        palette = "viridis",
        domain = usa_data$num2) 
      
      leaflet( data = usa_data) %>%
        # addTiles() %>%
        #setView(-74,42,zoom=6) %>%
        addPolygons(
          
          stroke = TRUE,
          smoothFactor = 1,
          fillOpacity = 0.7,
          weight = 1,
          color = "black",
          popup = ~htmltools::htmlEscape(paste0(Division.DB,":",num2)),
          label = ~paste0(Division.DB,":",num2),
          fillColor = ~pal2(num2)
          
        ) %>% addLegend("topright", pal = pal2, values = ~num2,
                        title = "Deaths <br/> by Division",
                        labFormat = leaflet::labelFormat(prefix = ""),
                        opacity = 1)
      
    })

  
  
}

shinyApp(ui, server)
