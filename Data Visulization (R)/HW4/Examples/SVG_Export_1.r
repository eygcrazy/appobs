##############################################################
# Example of exporting heat map to SVG
##############################################################

source('CalendarHeat.R')

stock = "MSFT"
start.date = "2013-01-1"
end.date = Sys.Date()
quote = paste("http://ichart.finance.yahoo.com/table.csv?s=",
               stock,
               "&a=", substr(start.date,6,7),
               "&b=", substr(start.date, 9, 10),
               "&c=", substr(start.date, 1,4), 
               "&d=", substr(end.date,6,7),
               "&e=", substr(end.date, 9, 10),
               "&f=", substr(end.date, 1,4),
               "&g=d&ignore=.csv", sep="")             
stock.data = read.csv(quote, as.is=TRUE)
head(stock.data)

calendarHeat(stock.data$Date, stock.data$Adj.Close, varname="MSFT Adjusted Close")

svg("MSFT.svg",width=14,height=7)
calendarHeat(stock.data$Date, stock.data$Adj.Close, 
             varname="MSFT Adjusted Close")
dev.off()

png("MSFT.png",type="cairo",width=1400,height=700)
calendarHeat(stock.data$Date, stock.data$Adj.Close, 
             varname="MSFT Adjusted Close")
dev.off()

