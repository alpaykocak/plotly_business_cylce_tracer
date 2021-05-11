library(rsdmx)
library(tidyverse)
library(zoo)
library(mFilter)
library(scales)
library(lubridate)
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

# gdpgrowth$Date <- as.Date(as.yearqtr(gdpgrowth$Date,format="%Y-Q%q"))

app_init_date <- if (month(init_date) == 1 & leap_year(init_date) == T) {init_date + 90 
} else if (month(init_date) == 1 & leap_year(init_date) == F) {init_date + 89 
} else if (month(init_date) == 2) {init_date + 89
} else if (month(init_date) == 3) {init_date + 90
} else { init_date + 91}
app_final_date <- if (month(final_date) == 1 & leap_year(final_date) == T) {final_date + 90 
} else if (month(final_date) == 1 & leap_year(final_date) == F) {final_date + 89 
} else if (month(final_date) == 2) {final_date + 89
} else if (month(final_date) == 3) {final_date + 90
} else { final_date + 91}

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

