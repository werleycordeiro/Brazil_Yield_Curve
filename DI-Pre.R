# Author: Werley Cordeiro
# werleycordeiro@gmail.com

# Function
yieldsbr = function(Initial_Date,Final_Date,Maturities){
 # Packages
 packages <- c("rvest","httr","functional")
 new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
 if(length(new.packages)) install.packages(new.packages)
 suppressMessages(library(rvest))
 suppressMessages(library(httr))
 suppressMessages(library(functional))

 dates = format(seq(as.Date(Initial_Date), as.Date(Final_Date), 'day'), format="%d-%m-%Y", tz="UTC")
 mat = matrix(NA,length(dates),length(Maturities))
 # Scraping
 for(i in 1:length(dates)){
 	di = GET(url = "http://www2.bmf.com.br/pages/portal/bmfbovespa/lumis/lum-taxas-referenciais-bmf-ptBR.asp",query = list(Data = dates[i]))
	data = read_html(di) %>% html_nodes("table") %>% html_nodes("td") %>% html_text()
	if(length(data)==0){i=i
	}else{
	data = data.frame(matrix(data, ncol=3, byrow=TRUE))
	data[,2] = as.numeric(gsub(",", ".", gsub("\\.", "", data[,2])))
	data[,3] = as.numeric(gsub(",", ".", gsub("\\.", "", data[,3])))
 # Spline
	t = as.integer(as.matrix(data[,1]))/21
	y = as.numeric(as.matrix(data[,2]))
	spl <- smooth.spline(y ~ t)
	t.new <- Maturities
	new<-predict(spl, t.new)
	mat[i,] = new$y
	}
	}
 colnames(mat) = paste0("M",Maturities)
 rownames(mat) = dates
 mat = mat[apply(mat, 1, Compose(is.finite, all)),]
 return(mat)
}

# Example
Initial_Date = '2019/06/28' # Available from 08/08/2003
Final_Date = '2019/07/01'
Maturities = c(1,3,6,9,12,15,18,21,24,27,30,33,36,39,48,60,72)

yields = yieldsbr(Initial_Date=Initial_Date,Final_Date=Final_Date,Maturities=Maturities)
yields
