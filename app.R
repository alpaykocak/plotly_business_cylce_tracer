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
rm(list=ls())
hamveri <- readxl::read_xls("deneme.xls")
colnames(hamveri)[1] <- "tarih"
newnames <- substr(colnames(hamveri)[-1],start = 23,stop = 26)
colnames(hamveri)[-1] <- newnames
nzv <- caret::nzv(hamveri)
hamveri <- hamveri[,-nzv]
table(colSums(sapply(hamveri[-181,], is.na)))
hamveri <- hamveri[-c(1:88,181),]
hamveri <- hamveri[,-c(26,37)]
table(colSums(sapply(hamveri[-181,], is.na)))
hamveri <- ts(hamveri[,-1], frequency = 4, start = c(1997,1))
newnames <- colnames(hamveri)
my <- apply(hamveri,2,function(x){ 
    res <- hpfilter(x,1600)
    cyc <- res$cycle
    return(cyc)
})
my <- ts(my, frequency = 4, start = c(1997,1))
mx <- diff(my)
my <- apply(my, 2, function(x){ rescale_mid(x,to = c(-2.95,2.95),mid = 0)})
my <- ts(my, frequency = 4, start = c(1997,1))
my <- window(my,start = c(1997,2))
mx <- apply(mx, 2, function(x){ rescale_mid(x,to = c(-2.95,2.95),mid = 0)})
mx <- ts(mx, frequency = 4, start = c(1997,2))
tarih <- seq(as.Date("1997/4/1"), by = "quarter", length.out = dim(mx)[1])
my <- as.data.frame(my) %>% mutate("tarih" = tarih)
mx <- as.data.frame(mx) %>% mutate("tarih" = tarih)
date <- data.frame(tarih = seq(as.Date(head(tarih)[1]),as.Date(tail(tarih)[c(-1:-5)]),by = "days"))
my_long <- left_join(date,my) %>% fill_(fill_cols = colnames(left_join(date,my))[-1],.direction = "down")
mx_long <- left_join(date,mx) %>% fill_(fill_cols = colnames(left_join(date,mx))[-1],.direction = "down")
my <- my_long %>%  gather("degisken","y",-tarih) 
mx <- mx_long %>%  gather("degisken","x",-tarih) 
veri <- left_join(my,mx)
k <- as.numeric(length(unique(veri$degisken)))
l <- as.numeric(length(unique(veri$tarih)))

ui <- fluidPage(
    titlePanel("Business Cycle Tracer for European Countries"),
    sidebarLayout(
        sidebarPanel(
        wellPanel(
            sliderInput("slider", "Time", 
                        animate = T,step = 90,
                        min = as.Date(head(tarih)[1]),
                        max = as.Date(tail(tarih)[c(-1:-5)]),
                        value = as.Date(head(tarih)[1]),
                        timeFormat="%b %Y")
        )  
    ),
    mainPanel(
        shinydashboard::box(plotlyOutput("rectPlot"))
    )
    )
    )
    
    
  
server <- function(input, output) {
    n <- reactive({
        verin <- veri %>% dplyr::filter(tarih == as.Date(input$slider))
    })
    name <- reactive({
        paste0("Yıllar itibariyla ", input$slider)
    })
    output$rectPlot <- renderPlotly({
        verim <- as.data.frame(n())
        p1 <- ggplot() + 
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
                           shape = degisken, 
                           color = degisken), 
                       size = 5) +
            scale_shape_manual(values = rep(15:19, len = k)) +
            scale_color_manual(values = c(rep("darkblue", k/3+1),
                                          rep("blue", k/3),
                                          rep("steelblue4",k/3))) +
            theme(legend.position = "right",
                  panel.grid = element_blank(),
                  axis.title.x=element_blank(), 
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.title.y=element_blank(), 
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank(),
                  panel.background = element_blank()
            )
        ggplotly(p1, width = 900, height = 600)
    })
}
# Run the application 
shinyApp(ui = ui, server = server)