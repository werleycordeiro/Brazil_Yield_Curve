# Author: Werley Cordeiro
# werleycordeiro@gmail.com

install.packages(c("rvest","httr"))

library(httr)
library(rvest)

# Scraping
date = "05/10/2018" # First data available: "01/02/2004"
di = GET(url = "http://www2.bmf.com.br/pages/portal/bmfbovespa/lumis/lum-taxas-referenciais-bmf-ptBR.asp",query = list(Data = date))
di = read_html(di) %>% html_nodes("table")
data = di %>% html_nodes("td") %>% html_text()
data = data.frame(matrix(data, ncol=3, byrow=TRUE))
data[,2] = as.numeric(gsub(",", ".", gsub("\\.", "", data[,2])))
data[,3] = as.numeric(gsub(",", ".", gsub("\\.", "", data[,3])))
colnames(data) = c("Calendar Days","252 Days","360 Days")
head(data)
ts.plot(data[,2])

# Spline
t = as.integer(as.matrix(data[,1]))/21
y = as.numeric(as.matrix(data[,2]))
spl <- smooth.spline(y ~ t)
t.new <- c(1,3,6,9,12,15,18,21,24,27,30,33,36,39,48,60,72) # Maturities
new<-predict(spl, t.new)
plot(new, ylab = 'Yield', xlab = 'months', main = paste0('Yield Curve: ', date))
lines(new)

format(seq(as.Date('2019/01/02'), as.Date('2019/06/28'), 'day'), format="%d-%m-%Y", tz="UTC") # think in a loop...
