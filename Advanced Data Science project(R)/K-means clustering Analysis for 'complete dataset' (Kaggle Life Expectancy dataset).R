##   Partitioning Clustering        
##   K-means clustering
###  Foodstuffs example 
###########################################
library(foreign)
library(factoextra)
library(palettesForR)
library(ggplot2)
library(Rtsne)
library(gridExtra)
work_dir <- "/Users/appobs/Desktop/hw/424/hw4"
setwd(work_dir)

lf <- read.csv("/Users/appobs/Desktop/hw/424/hw4/completeData.csv")

## delete categorial number, since the CA does not work for them;
## Then to standarize the data;

lf2 = lf[, -c(1,2,4)]
df = scale(lf2)

## check out the data's info

head(df, n =5)
str(df)

## Determine the optimal number of cluster;

fviz_nbclust(df, kmeans, method = "wss") + geom_vline(xintercept = 5, linetype = 2)

## To set the seed number make sure my approach can be iterated:

set.seed(500)
km_result = kmeans(df, 5, nstart = 24)
print(km_result)

## Via the scatterplot matrix, faild since the figure margins to large;

pairs(lf2[,-1], panel=function(x,y) text(x,y,km_result$cluster))

## Combine the clusters with original dataset;

dd = cbind(lf2, cluster = km_result$cluster)
head(dd)

## To see the number of var in each clusters:

table(dd$cluster)

## to visualize the result;

colors = rainbow(length(unique(dd$cluster)))
names(colors) = unique(dd$cluster)

## Executing the algorithm on curated data
tsne = Rtsne(dd[,-1], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)
exeTimeTsne = system.time(Rtsne(dd[,-1], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500))

## Plotting
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=dd$cluster, col=colors[dd$cluster])

############################ ############################ 
# this approach only works for lower dimensional dataset

##  the graphics device was messed up earlier somehow by 
## exporting some graphics and it didn't get reset
#dev.off()

#fviz_cluster(km_result, data = df, 
#             ellipse.type = "euclid",
#             repel = TRUE,
#             ggtheme = theme_minimal())

############################ ############################ 


## this approach works for the high dimensional dataset(unknow error...)
############################ ############################ 
#plot_cluster=function(dd, var_cluster, palette)  
#{
#  ggplot(dd, aes_string(x="V1", y="V2", color=var_cluster)) +
#    geom_point(size=0.25) +
#    guides(colour=guide_legend(override.aes=list(size=6))) +
#    xlab("") + ylab("") +
#    ggtitle("") +
#    theme_light(base_size=20) +
#    theme(axis.text.x=element_blank(),
#          axis.text.y=element_blank(),
#          legend.direction = "horizontal", 
#          legend.position = "bottom",
#          legend.box = "horizontal") + 
#    scale_colour_brewer(palette = palette) 
# }


# plot_k=plot_cluster(lf2, "cl_kmeans", "Accent")  
# plot_h=plot_cluster(lf2, "cl_hierarchical", "Set1")

## and finally: putting the plots side by side with gridExtra lib...

# grid.arrange(plot_k, plot_h,  ncol=2)
############################ ############################ 