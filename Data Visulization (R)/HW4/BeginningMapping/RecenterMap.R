require(plyr)
RecenterMap = function(
                  data=world,
                  center=260,
                  # positive values only - US centered view is 260
                  shapeType=c("polygon","segment"),
                  idfield=NULL
                ) 
{
  
  #use inherited id column, or create a new one from breaks in the data
  if(is.null(idfield)) {
    data$id=factor(cumsum(is.na(data$long)))
  }else{
    data$id = get(idfield,pos=data)
  }
  
  # shift coordinates to recenter worldmap
  data$long <- ifelse(data$long <= center - 180 , data$long + 360, data$long)
  
  ### Function to regroup split lines and polygons
  # takes dataframe, column with long and unique group variable, 
  #returns df with added column named group.regroup
  RegroupElements <- function(df, longcol, idcol){
    g <- rep(1, length(df[,longcol]))
    if (diff(range(df[,longcol])) > 300) { 
      # check if longitude within group differs more than 300 deg, ie if element was split
      d <- df[,longcol] > mean(range(df[,longcol])) 
      # we use the mean to help us separate the extreme values
      g[!d] <- 1 
      # some marker for parts that stay in place (we cheat here a little, as we do not take into account concave polygons)
      g[d] <- 2 
      # parts that are moved
    }
    g <- paste(df[, idcol], g, sep=".") # attach to id to create unique group variable for the dataset
    df$group.regroup <- g
    df
  }
  
  ### Function to close regrouped polygons
  # takes dataframe, checks if 1st and last longitude value are the same, if not, inserts first as last and reassigns order variable
  ClosePolygons <- function(df, longcol, ordercol){
    if (df[1,longcol] != df[nrow(df),longcol]) {
      tmp <- df[1,]
      df <- rbind(df,tmp)
    }
    df
  }
  
  # now regroup
  
  if(shapeType[1]=="segment") {
    Gap = abs(c(0,data$long[-1]-data$long[-nrow(data)]))
    split = factor(cumsum(Gap>=300))
    splitVoyages = factor(as.numeric(factor(paste(split,get(idfield,pos=data)))))
    data$group=splitVoyages
  }
  # close polys
  if(shapeType[1]=="polygon") {
    data <- ddply(data, .(id), RegroupElements, "long", "id")
    data <- ddply(data, .(group.regroup), ClosePolygons, "long", "order") # use the new grouping var
    data$group = data$group.regroup
  }
  data
}