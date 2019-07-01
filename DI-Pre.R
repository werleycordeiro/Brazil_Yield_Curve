install.packages(c("rvest","httr"))

library(httr)
library(rvest)

data = "06/28/2019" # First date available: "01/02/2004"
di = GET(url = "http://www2.bmf.com.br/pages/portal/bmfbovespa/lumis/lum-taxas-referenciais-bmf-enUS.asp",query = list(Data = data))
di = read_html(di) %>% html_nodes("table")
data = di %>% html_nodes("td") %>% html_text()
data = data.frame(matrix(data, ncol=3, byrow=TRUE))
colnames(data) = c("Calendar Days","252 Days","360 Days")
head(data)
ts.plot(data[,2])
