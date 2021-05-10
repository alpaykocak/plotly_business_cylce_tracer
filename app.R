library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(readr)
library(caret)
library(tidyr)
library(scales)
library(mFilter)
library(tidyverse)
library(lubridate)
library(shinydashboard)
library(zoo)
library(rsdmx)
rm(list=ls())
url <-"https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/QNA/AUS+AUT+BEL+CAN+CHL+COL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LTU+LVA+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+EA19+EU27_2020+EU15+G-7+NAFTA+OECDE+G-20+OECD+OTF+NMEC+ARG+BRA+BGR+CHN+CRI+IND+IDN+ROU+RUS+SAU+ZAF.B1_GE.GYSA.Q/all?startTime=1997-Q1&endTime=2021-Q1"
gdpgrowth <- readSDMX(url)
gdpgrowth <- as.data.frame(gdpgrowth)
gdpgrowth <- gdpgrowth %>% select("Countries" = LOCATION,"Date" = obsTime, "Veri" = obsValue)
ulkeler <- unique(gdpgrowth$Countries)
istenen <- c( "JPN", "KOR", "MEX", 
              "TUR", "GBR", "USA",  "BRA",
              "CHN", "RUS", "SAU", "DEU", 
              "FRA", "CAN","ISR", "IND")
gdpgrowth$Date <- as.Date(as.yearqtr(gdpgrowth$Date,format="%Y-Q%q"))
gdpgrowth <- gdpgrowth %>% 
  filter(Countries %in% istenen & Date >= as.Date("2016-01-01") & Date < as.Date("2021-01-01")) %>% 
  spread(Countries,Veri) %>% 
  select(-Date)
datum <- data.frame(tarih = seq(as.Date("2016-03-31"),as.Date("2020-12-31"),by = "quarters")) 
my <- apply(gdpgrowth,2,function(x){ 
  res <- hpfilter(x,1600)
  cyc <- res$cycle
  return(cyc)
})
date_sim <- data.frame(tarih = seq(as.Date(head(datum$tarih)[1]),as.Date(tail(datum$tarih)[c(-1:-5)]),by = "days"))
my <- cbind(datum,as.data.frame(my)) %>% 
  right_join(date_sim) %>%
  arrange(tarih) %>%
  mutate_if(is.numeric, na.approx)
myts <- ts(my[,-1], frequency = 365.25, start = c(2016,31,3))
mxts <- diff(myts)
my[,-1] <- apply(my[,-1], 2, function(x){ rescale_mid(x,to = c(-2.95,2.95),mid = 0)})
my <- my[-1,]
mx <- cbind("tarih" = my$tarih, as.data.frame(apply(mxts, 2, function(x){ rescale_mid(x,to = c(-2.95,2.95),mid = 0)})))
my_long <- my %>%  gather("degisken","y",-tarih) 
mx_long <- mx %>%  gather("degisken","x",-tarih) 
veri <- left_join(my_long,mx_long)
tarih <- unique(veri$tarih)
k <- as.numeric(length(unique(veri$degisken)))
l <- as.numeric(length(unique(veri$tarih)))
ui <- dashboardPage(
  dashboardHeader(title = "Business Cycle Tracer for European Countries"),
    dashboardSidebar(
      width = 100,
        sidebarMenu(
          menuItem("Dashboard", tabName = "dashboard"),
          menuItem("Raw data", tabName = "rawdata")
        )  
    ),
  
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                box(
                  width = 7, status = "info", solidHeader = TRUE,
                  title = "Time interval",
                  sliderInput("slider", "Time", 
                            animate = T,step = 10,
                            min = as.Date(head(tarih)[1]),
                            max = as.Date(tail(tarih)[c(-1:-5)]),
                            value = as.Date(head(tarih)[1]),
                            timeFormat="%b %Y")),
                valueBoxOutput(width = 2,"count"),
                valueBoxOutput(width = 2,"users"),
                valueBoxOutput(width = 2,"devices"),
                valueBoxOutput(width = 2,"tools"),
                valueBoxOutput(width = 2,"days")
              ),
              fluidRow(
                box(
                  width = 7, status = "info", solidHeader = TRUE,
                  title = "Business cycle situtation of the countries",
                  plotlyOutput("rectPlot", width = "100%", height = 600)
                ),
                box(
                  width = 2, status = "info",solidHeader = TRUE,
                  title = "Situation of countries",
                  div(tableOutput("packageTable1"), style = "font-size:90%")
                ),
                box(
                  width = 2, status = "info",solidHeader = TRUE,
                  title = "Situation of countries",
                  div(tableOutput("packageTable2"), style = "font-size:90%")
                )
              ) 
      
        ),
      tabItem("rawdata",
              numericInput("maxrows", "Rows to show", 25),
              verbatimTextOutput("rawtable"),
              downloadButton("downloadCsv", "Download as CSV")
      )
    )
    )
    )
    
    
  
server <- function(input, output) {
  cond <- c("Recession", "Recovery", "Slowdown", "Expansion")
    n <- reactive({
        verin <- veri %>% dplyr::filter(tarih == as.Date(input$slider))
    })
    output$packageTable1 <- renderTable(
      n() %>%
        select("Date" = tarih, "Country" = degisken,x,y) %>%
        mutate("Date" = as.character(zoo::as.yearmon(Date)) ,
          "State" = ifelse(x<=0 & y<0,"Recession",
                                ifelse(x>0 & y<=0,"Recovery",
                                       ifelse(x<=0 & y>0,"Slowdown",
                                              ifelse(x>0 & y>=0,"Expansion",""))))) %>%
        select(-x,-y) %>%
        slice(1:ceiling(k/2)))
    output$packageTable2 <- renderTable(
      n() %>%
        select("Date" = tarih, "Country" = degisken,x,y) %>%
        mutate("Date" = as.character(zoo::as.yearmon(Date)) ,
               "State" = ifelse(x<=0 & y<0,"Recession",
                                ifelse(x>0 & y<=0,"Recovery",
                                       ifelse(x<=0 & y>0,"Slowdown",
                                              ifelse(x>0 & y>=0,"Expansion",""))))) %>%
        select(-x,-y) %>%
        slice((ceiling(k/2)+1):k)
      )
    
    statetable <- reactive({
      tablo <- veri %>% dplyr::filter(tarih == as.Date(input$slider)) %>%
        select("Date" = tarih, "Country" = degisken,x,y) %>%
        mutate("Date" = as.character(zoo::as.yearmon(Date)) ,
               "State" = ifelse(x<=0 & y<0,"Recession",
                                ifelse(x>0 & y<=0,"Recovery",
                                       ifelse(x<=0 & y>0,"Slowdown",
                                              ifelse(x>0 & y>=0,"Expansion",""))))) %>%
        select(-x,-y) %>% select(State) %>%
        as.data.frame() %>% table(.)
    })
    
    output$days <- renderValueBox({
      valueBox(
        value = as.character(as.yearmon(input$slider)),
        subtitle = as.character("Date"),
        color = "blue",
        icon = icon("download")
      )
    })
    
    output$count <- renderValueBox({
      valueBox(
        value = tryCatch(as.character(paste0(c(statetable())[names(c(statetable())) == "Recession"][[1]]," country/s")),  error = function(e) as.character(paste0(0," country"))),
        subtitle = as.character("in Recession"),
        color = "red",
        icon = icon("arrow-down")
      )
    })
    output$users <- renderValueBox({
      valueBox(
        value = tryCatch(as.character(paste0(c(statetable())[names(c(statetable())) == "Recovery"][[1]]," country/s")),  error = function(e) as.character(paste0(0," country"))),
        subtitle = as.character("in Recovery"),
        color = "yellow",
        icon = icon("arrow-right")
      )
    })
    output$devices <- renderValueBox({
      valueBox(
        value = tryCatch(as.character(paste0(c(statetable())[names(c(statetable())) == "Slowdown"][[1]]," country/s")),  error = function(e) as.character(paste0(0," country"))),
        subtitle = as.character("in Slowdown"),
        color = "orange",
        icon = icon("arrow-left")
      )
    })
    output$tools <- renderValueBox({
      valueBox(
        value = tryCatch(as.character(paste0(c(statetable())[names(c(statetable())) == "Expansion"][[1]]," country/s")),  error = function(e) as.character(paste0(0," country"))),
        subtitle = as.character("in Expansion"),
        color = "green",
        icon = icon("arrow-up")
      )
    })
    name <- reactive({
        paste0("Yıllar itibariyla ", input$slider)
    })
    output$rectPlot <- renderPlotly({
        verim <- as.data.frame(n())
        p1 <- ggplot() + 
            annotate("text", x = 2.55, y = 2.75, label = "Expansion") + 
            annotate("text", x = -2.55, y = 2.75, label = "Slowdown") + 
            annotate("text", x = -2.55, y = -2.75, label = "Recession") + 
            annotate("text", x = 2.55, y = -2.75, label = "Recovery") + 
            geom_rect(data=data.frame(xmin = -3, xmax = -0, ymin = -3, ymax = 0),
                      aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red", alpha=0.5) +
            geom_rect(data=data.frame(xmin = -3, xmax = -0, ymin = 0, ymax = 3),
                      aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="orange", alpha=0.5) +
            geom_rect(data=data.frame(xmin = 0, xmax = 3, ymin = 0, ymax = 3),
                      aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.5) +
            geom_rect(data=data.frame(xmin = 0, xmax = 3, ymin = -3, ymax = 0),
                      aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="yellow", alpha=0.5) +
            geom_point(data = verim,
                       aes(x = x, 
                           y = y,
                           text = paste('</br>Country: ', degisken),
                           shape = degisken, 
                           color = degisken), 
                       size = 5) +
            scale_shape_manual(name = "", values = rep(15:19, len = k)) +
            scale_color_manual(name = "", values = c(rep("darkblue", ceiling(k/3)),
                                          rep("blue", ceiling(k/3)),
                                          rep("steelblue4",floor(k/3)))) +
            theme(legend.position = "right",
                  panel.grid = element_blank(),
                  axis.title.x=element_blank(), 
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.title.y=element_blank(), 
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank(),
                  panel.background = element_blank()
            ) +
          guides(color = guide_legend(override.aes = list(size = 3) ) )
        ggplotly(p1, width = 900, height = 600,tooltip = c("text")) %>% layout(legend = list(orientation = 'v'))
    })
}
# Run the application 
shinyApp(ui = ui, server = server)