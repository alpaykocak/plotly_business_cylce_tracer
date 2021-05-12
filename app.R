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
library(readxl)

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
  spread(Countries,Veri)
SAU_INPUT <- read_excel("SAU_INPUT.xlsx")
SAU_INPUT <- SAU_INPUT %>% select(Date, "SAU" = SAU_GROWTH) %>% na.omit(.) %>% mutate(Date = as.Date(Date))
gdpgrowth$SAU[match(SAU_INPUT$Date,as.Date(as.yearqtr(gdpgrowth$Date,format="%Y-Q%q")))] <- SAU_INPUT$SAU
gdpgrowth <- gdpgrowth %>% 
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
      width = 150,
      sidebarMenu(
        menuItem("What is this?", tabName = "definition", icon = icon("book-open")),
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Results", tabName = "rawdata", icon = icon("th"))
        )  
    ),
  
  dashboardBody(
    tabItems(
      tabItem("definition",
              fluidRow(
              
                box(status = "info",width = 4,
                h1("Business Cycle Dashboard"),
                
                p("Business Cycle Dashboard is a tool designed to help show the history and current state of economies. 
                Just as the clock shows the current time, Business Cycle Dashboard shows what the cycle of the economy is. 
                Business Cycle Dashboard follows the cyclical nature of economic developments. 
                In cyclical fluctuations; the high growth period is followed by the slow growth period and the subsequent shrinkage period is followed by the recovery period.
                The situation of the conjuncture for selected countries and country groups is determined using real GDP adjusted for 
                  seasonal and calendar effects. By collectively submitting the results to fifteen country/country groups,
                  a general picture of the economy on an international scale is taken for a moment in time."),
                p("Business Cycle Dashboard can be found in four different situations."),
                p("- Trend over and descending (on the left-upper side of the dial, in orange),") ,
                p("- Under the trend and descending (on the left-bottom of the dial, in red),"),
                p("- Under the trend and ascending (on the right-bottom of the dial, in yellow),"),
                p("- Trend above and ascending (on the upper right-hand side of the dial, in green),"),
                p("Each country / country group is represented by a dot on the quadrant, and the location (coordinate) of that point changes depending on time."),
                p("To obtain Business Cycle Dashboard, the conjuncture component is obtained for the GDP of each country. 
                  The conjuncture component is the deviations from the long-term trend variable. 
                  The conjuncture component provides information about whether an indicator is above or below the trend and whether it is increasing or decreasing compared to the previous period. 
                  The four quadrants on the Business Cycle Dashboard show the four phases of business cycle fluctuations.")),
             
                     box(status = "info",width = 4,
                         h1("Detail Explanation"),
                p("Closely following national and international developments in the field of economy, 
                This Company LTD. CO. Makes its Business Cycle Dashboard system developed to facilitate the analysis of 
                medium-term economic developments for the public. Seasonal and calendar adjusted real GDP, 
                when focusing on cyclical fluctuations (medium-term movements) after short-term, 
                periodic and irregular movements are cleared, the message received from 
                Seasonal and calendar adjusted real GDP becomes clear. Interpretation of 
                cyclical fluctuations becomes easier with a dynamic graphical presentation. 
                Combining seasonal and calendar adjusted real GDPs in such a system has two important advantages. 
                First, it is possible to analyze individual movements of seasonal and calendar adjusted real GDP and 
                their comparative movements with other countries."),
                p("Cyclical fluctuations have been the subject of many scientific studies and discussions. 
                  In order to reveal the ideas put forward on this subject until today, 
                  a large literature research has been carried out in the national and international field. 
                  When the literature is reviewed, it is seen that there is no consensus on the nature and causes of cyclical fluctuations."),
                p("This difference is clearly understood in the studies of Zarnowitz (1987), Prescott (1986), Cooper (1997), Fuhrer and Schuh (1998).
                Considering that there is no consensus on the theoretical basis of cyclical fluctuations, 
                it is quite clear that measuring the current situation of the economy will be a controversial issue. 
                This situation basically turns into a discussion on how to define cyclical fluctuations.")), 
              
                     box(status = "info",width = 4,
                         h1("Literature"),
                p("Although there are many different opinions on this issue, many economists and researchers refer to 
                Burns and Mitchell's (1946) definition of business cycles. According to this definition:"),
                p("'Business cycle fluctuations are the fluctuations experienced in the total economic activities of countries dominated by the private sector. 
                  A 'conjuncture' of many economic activities at the same time; it covers the periods of expansion, 
                  then deceleration, contraction and recovery (recovery) to move to the next cycle of the cycle. 
                  This process is repeated in the given order, but is not periodic.'"),
                p("In addition, the definition of recession put forward by the United States National Bureau of Economic Research (NBER) 
                is also very popular (Christiano and Fitzgerald, 1998):"),
                p("'...recession; defined as a permanent period of decline in aggregate production, income, employment and trade, usually lasting from six months to a year, and is marked by widespread contractions in many sectors of the economy ...'"),
                p("Various methods are available to define and measure business cycle fluctuations. 
                The method we use is basically a harmonized method based on Hodrick and Prescott (1997).
                Although there are many different definitions, it can be said that there is a consensus 
                in the literature about some features of conjuncture fluctuations 
                (Christiano & Fitzgerald, 1998; Banerji & Hiris, 2001; Klein & Moore, 1982; Stock & Watson, 1988; Zarnowitz, 1987)."),
                p("The features agreed upon;"),
                p("- Business cycle fluctuations are defined as the joint movements of many economic variables. Fluctuations should be evident in expenditure and labor market indicators as well as in production-based indicators. In this respect, the condition of 'prevalence' is a necessary condition. To put it more clearly, the fluctuation should be felt not only in a particular sector but throughout the economy (in many sectors)."),
                p("- Business cycle fluctuations are repetitive but non-periodic. To be more precise, the transition sequence between the periods of expansion and contraction continues in the same order, but the time intervals between these transitions need not be regular."),
                p("- Business cycle fluctuations must be precise and continuous; small and short movements do not reflect this situation."),
                p("International examples are",
                a(href="https://www.cbs.nl/en-gb/visualisations/business-cycle-tracer", 
                  "CBS Netherlands",target="_blank"),
                ", ",
                a(href="https://ec.europa.eu/economy_finance/graphs/2014-07-07_business_cycle_en.htm", 
                  "EU Commission",target="_blank"),
                "and ",
                a(href="https://ec.europa.eu/eurostat/cache/bcc/bcc.html", 
                  "Eurostat",target="_blank"),
                "."
                )
                )
              )
              ),
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