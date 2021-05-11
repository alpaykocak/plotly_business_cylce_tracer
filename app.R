library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(readr)
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
istenen <- c( "JPN", "MEX", "OECD","NAFTA",
              "TUR", "GBR", "USA",  "BRA",
              "CHN", "RUS", "SAU", "EU27_2020", 
              "ARG",  "G-20", "IND")
isimler <- c("Japan", "Mexico", "OECD","NAFTA",
             "Turkey","United Kingdom", "United States", "Brazil",
             "China", "Russia", "Saudi Arabia", "EU27",
             "Argentina", "G20", "India")
names(istenen) <- isimler
zaman <- unique(as.Date(as.yearqtr(gdpgrowth$Date,format="%Y-Q%q")))
lenght_date <- length(zaman)
init_date <- sort(zaman)[1]
final_date <- sort(zaman)[lenght_date]
gdpgrowth <- gdpgrowth %>% 
  filter(Countries %in% istenen) %>% 
  spread(Countries,Veri) %>%
  select(-Date)
colnames(gdpgrowth) <- names(sort(istenen))
gdpgrowth <- gdpgrowth[,sort(colnames(gdpgrowth))]
my <- apply(gdpgrowth,2,function(x){ 
  filtered.x <- x
  filtered.x[!is.na(x)] <- (hpfilter(x[!is.na(x)],1600))$cycle
  return(filtered.x)
})

app_init_date <- if (quarter(init_date) == 1 & leap_year(init_date) == T) {init_date + 88 
} else if (quarter(init_date) == 1 & leap_year(init_date) == F) {init_date + 88 
} else if (quarter(init_date) == 2) {init_date + 88
} else if (quarter(init_date) == 3) {init_date + 88
} else { init_date + 88}
app_final_date <- if (quarter(final_date) == 1 & leap_year(final_date) == T) {final_date + 88 
} else if (quarter(final_date) == 1 & leap_year(final_date) == F) {final_date + 88 
} else if (quarter(final_date) == 2) {final_date + 88
} else if (quarter(final_date) == 3) {final_date + 88
} else { final_date + 88}

datum <- data.frame(tarih = seq(app_init_date,app_final_date,by = "quarters")) 
date_sim <- data.frame(tarih = seq(app_init_date,app_final_date,by = "days"))

my <- cbind(datum,as.data.frame(my)) %>% 
  right_join(date_sim) %>%
  arrange(tarih) 

for (i in colnames(my)[-1]) {
  serie_init_ind = which(my[,i] == na.omit(my[,i])[1], arr.ind=TRUE)
  serie_final_ind = which(my[,i] == na.omit(my[,i])[length(na.omit(my[,i]))], arr.ind=TRUE)
  my[serie_init_ind:serie_final_ind,i] <- na.approx(my[,i][serie_init_ind:serie_final_ind])
}

mxts <- diff(ts(my[,-1]))
my[,-1] <- apply(my[,-1], 2, function(x){ rescale_mid(x,to = c(-2.95,2.95),mid = 0)})
my <- my[-1,]
mx <- cbind("tarih" = my$tarih, as.data.frame(apply(mxts, 2, function(x){ rescale_mid(x,to = c(-2.95,2.95),mid = 0)})))
my_long <- my %>%  gather("degisken","y",-tarih) 
mx_long <- mx %>%  gather("degisken","x",-tarih) 
veri <- left_join(my_long,mx_long)
tarih <- unique(veri$tarih)
k <- as.numeric(length(unique(veri$degisken)))



ui <- dashboardPage(
  dashboardHeader(title = "Business Cycle Dashboard",titleWidth = 500),
    dashboardSidebar(
      width = 100,
        sidebarMenu(
          menuItem("Dashboard", tabName = "dashboard"),
          menuItem("Results", tabName = "rawdata")
        )  
    ),
  
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                box(
                  width = 7, status = "info", solidHeader = TRUE,
                  title = "Time interval",
                  sliderInput("slider", "Month", 
                            animate = T,step = 10,
                            min = as.Date(head(tarih)[1]),
                            max = as.Date(tail(tarih)[c(-1:-5)]),
                            value = as.Date(tail(tarih)[c(-1:-5)]),
                            timeFormat="%b %Y")),
                valueBoxOutput(width = 2,"count"),
                valueBoxOutput(width = 2,"users"),
                valueBoxOutput(width = 2,"devices"),
                valueBoxOutput(width = 2,"tools")
              ),
              fluidRow(
                valueBoxOutput(width = 2,"days")
              ),
              fluidRow(
                box(
                  width = 7, status = "info", solidHeader = TRUE, footer = "Calculations based on OECD Data.",
                  title = "Business Cycle Situtation of the Selected Countries",
                  plotlyOutput("rectPlot", width = "100%", height = 600)
                ),
               
                box(
                  width = 2, status = "info",solidHeader = TRUE,footer = "Calculations based on OECD Data.",
                  title = "Countries in Recession",background = "red",
                  div(tableOutput("packageTable1"), style = "font-size:100%")
                ),
                box(
                  width = 2, status = "info",solidHeader = TRUE,footer = "Calculations based on OECD Data.",
                  title = "Countries in Recovery",background = "yellow",
                  div(tableOutput("packageTable2"), style = "font-size:100%")
                ),
                box(
                  width = 2, status = "info",solidHeader = TRUE,footer = "Calculations based on OECD Data.",
                  title = "Countries in Slowdown",background = "orange",
                  div(tableOutput("packageTable3"), style = "font-size:100%")
                ),
                box(
                  width = 2, status = "info",solidHeader = TRUE,footer = "Calculations based on OECD Data.",
                  title = "Countries in Expansion", background = "green",
                  div(tableOutput("packageTable4"), style = "font-size:100%")
                )
              )
      
        ),
      tabItem("rawdata",
              fluidRow(width=10,
              hr(),
              column(width = 3,
                     box(
                       status = "info", solidHeader = TRUE,
                       title = "Country",       
              selectInput("country",label = "Select Country",choices = sort(isimler),multiple = F,selected = sort(isimler)[1])
                     )
              ),
              box(
                  width = 5, status = "info", solidHeader = TRUE,footer = "Source: OECD Data.",
                  title = "Growth rates of the Selected Countries",
                  plotOutput("graph")
                ),
              box(
                width = 4, status = "info",solidHeader = TRUE,footer = "Source: OECD Data. Calculations based on OECD Data.",
                title = "Countries Data",
                DT::dataTableOutput("tablo")
              )
              )

      )
    )
    )
    )
    
    
  
server <- function(input, output) {
  cond <- c("Recession", "Recovery", "Slowdown", "Expansion")
    n <- reactive({
        verin <- veri %>% dplyr::filter(tarih == as.Date(input$slider))
    })
    
    ozettable <- reactive({
      tablo <- n() %>%
        select("Date" = tarih, "Country" = degisken,x,y) %>%
        mutate("Date" = as.character(zoo::as.yearmon(Date)) ,
               "State" = ifelse(x<=0 & y<0,"Recession",
                                ifelse(x>0 & y<=0,"Recovery",
                                       ifelse(x<=0 & y>0,"Slowdown",
                                              ifelse(x>0 & y>=0,"Expansion",""))))) %>%
        select(-x,-y)
    })
    
    statetable <- reactive({
      tablo <- ozettable() %>% select(State) %>%
        as.data.frame() %>% table(.)
    })
    
    output$packageTable1 <- renderTable(
      ozettable() %>%
        filter(State == "Recession") %>% select(Country)
      )
    output$packageTable2 <- renderTable(
      ozettable() %>%
        filter(State == "Recovery") %>% select(Country)
    )
    
    output$packageTable3 <- renderTable(
      ozettable() %>%
        filter(State == "Slowdown") %>% select(Country)
    )
    
    output$packageTable4 <- renderTable(
      ozettable() %>%
        filter(State == "Expansion") %>% select(Country)
    )
    
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
                  plot.background = element_rect(fill = "white"),
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
    
    result <- veri %>%
      select("Date" = tarih, "Country" = degisken,x,y) %>%
      mutate(
        "State" = ifelse(x<=0 & y<0,"Recession",
                         ifelse(x>0 & y<=0,"Recovery",
                                ifelse(x<=0 & y>0,"Slowdown",
                                       ifelse(x>0 & y>=0,"Expansion","NA"))))) %>%
      select(-x,-y) %>% spread(Country,State) %>% filter(Date %in% datum$tarih) %>% bind_cols(gdpgrowth[-1,])
    colnames(result) <- c("Date",paste0("State@",sort(names(istenen))),paste0("Growth@",sort(names(istenen))))
    result <- result %>% gather("type","data",-Date) %>% 
      separate(type,c("type2","Country"),"@") %>% spread(type2,data) %>% arrange(Country,Date) %>% 
      mutate("Growth" = round(as.numeric(Growth),2)) %>% filter( complete.cases(.))
    
    tablo <- reactive({
      m <- result %>% filter(Country == input$country)
    })
    
    output$tablo  <- DT::renderDataTable({
      
      DT::datatable(
        { tablo() %>% mutate(Date = paste0(year(Date),"-",quarter(Date)))},
        caption = "This table presents growth rate and corresponding business cycle sitation ",  
        extensions = 'Buttons',
        
        options = list(
          pageLength = as.numeric(length(unique(result$Date))),
          paging = TRUE,
          searching = TRUE,
          fixedColumns = FALSE,
          autoWidth = FALSE,
          ordering = TRUE,
          dom = 'ftpBRSQ',
          buttons = c('copy', 'csv', 'excel')
        ),
        
        class = "display"
      )
    })
    
    output$graph <- renderPlot({
      result %>% filter(Country == input$country) %>% ggplot(aes(Date,Growth,group = Country,fill = Country)) + geom_line()
    })
}
# Run the application 
shinyApp(ui = ui, server = server)