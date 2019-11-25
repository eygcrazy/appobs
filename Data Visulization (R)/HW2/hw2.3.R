library(ggplot2)
library(tidyverse)
library(readr)
library(vioplot)
library(vioplot)
library(devtools)
library(digest)

setwd("/Users/appobs/Desktop/hw/465")
pe = read.csv("/Users/appobs/Desktop/hw/465/week4/hw2/PerceptionExperiment.csv")
head(pe)
pe$error = pe$Response - pe$TrueValue
head(pe)

# for test
toPe = pe[ ,1]
rownames(pe)
names(toPe) = rownames(pe)
head(toPe)
toPe = as.numeric(toPe)
dotchart(sort(toPe), pch = 16, main = "level of Test")

# create a theme for dot plots, which can be reused
theme_dotplot = theme_bw(14) +
  theme(axis.text.y = element_text(size = rel(.75)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(.75)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5),
        panel.grid.minor.x = element_blank())

# create the plot
head(pe)
ggplot(pe, aes(x = error, y = reorder(Test,error ))) +
  geom_point(color = 'purple') +
  scale_x_continuous(limits = c(35, 95),
                     breaks = seq(-1,1,0.2)) +
  theme_dotplot +
  xlab('Median of error') +
  ylab('test') +
  ggtitle('median error vs test')

#jitter & distribution
ggplot(pe, aes(x=Test, y=Abserror)) + geom_boxplot(aes(fill=Test)) + 
  geom_jitter(color="red", alpha=.3, size=3, width=.2) +
  geom_point(aes(x=as.numeric(Test) + rnorm(n.each, 0, .03), y=Abserror), color="red", alpha=.3, size=3)

#3.d
vioplot2 <- function (x, ..., range = 1.5, h = NULL, ylim = NULL, names = NULL, 
                      horizontal = FALSE, col = "magenta", border = "black", lty = 1, 
                      lwd = 1, rectCol = "black", colMed = "white", pchMed = 19, 
                      at, add = FALSE, wex = 1, drawRect = TRUE, side="both") 
{
  datas <- list(x, ...)
  n <- length(datas)
  if (missing(at)) 
    at <- 1:n
  upper <- vector(mode = "numeric", length = n)
  lower <- vector(mode = "numeric", length = n)
  q1 <- vector(mode = "numeric", length = n)
  q2 <- vector(mode = "numeric", length = n)
  q3 <- vector(mode = "numeric", length = n)
  med <- vector(mode = "numeric", length = n)
  base <- vector(mode = "list", length = n)
  height <- vector(mode = "list", length = n)
  baserange <- c(Inf, -Inf)
  args <- list(display = "none")
  radj <- ifelse(side == "right", 0, 1)
  ladj <- ifelse(side == "left", 0, 1)
  if (!(is.null(h))) 
    args <- c(args, h = h)
  med.dens <- rep(NA, n)
  for (i in 1:n) {
    data <- datas[[i]]
    data.min <- min(data)
    data.max <- max(data)
    q1[i] <- quantile(data, 0.25)
    q2[i] <- quantile(data, 0.5)
    q3[i] <- quantile(data, 0.75)
    med[i] <- median(data)
    iqd <- q3[i] - q1[i]
    upper[i] <- min(q3[i] + range * iqd, data.max)
    lower[i] <- max(q1[i] - range * iqd, data.min)
    est.xlim <- c(min(lower[i], data.min), max(upper[i], 
                                               data.max))
    smout <- do.call("sm.density", c(list(data, xlim = est.xlim), 
                                     args))
    med.dat <- do.call("sm.density", 
                       c(list(data, xlim=est.xlim,
                              eval.points=med[i], display = "none")))
    med.dens[i] <- med.dat$estimate
    hscale <- 0.4/max(smout$estimate) * wex
    base[[i]] <- smout$eval.points
    height[[i]] <- smout$estimate * hscale
    med.dens[i] <- med.dens[i] * hscale
    t <- range(base[[i]])
    baserange[1] <- min(baserange[1], t[1])
    baserange[2] <- max(baserange[2], t[2])
  }
  if (!add) {
    xlim <- if (n == 1) 
      at + c(-0.5, 0.5)
    else range(at) + min(diff(at))/2 * c(-1, 1)
    if (is.null(ylim)) {
      ylim <- baserange
    }
  }
  if (is.null(names)) {
    label <- 1:n
  }
  else {
    label <- names
  }
  boxwidth <- 0.05 * wex
  if (!add) 
    plot.new()
  if (!horizontal) {
    if (!add) {
      plot.window(xlim = xlim, ylim = ylim)
      axis(2)
      axis(1, at = at, label = label)
    }
    box()
    for (i in 1:n) {
      polygon(x = c(at[i] - radj*height[[i]], rev(at[i] + ladj*height[[i]])), 
              y = c(base[[i]], rev(base[[i]])), 
              col = col, border = border, 
              lty = lty, lwd = lwd)
      if (drawRect) {
        lines(at[c(i, i)], c(lower[i], upper[i]), lwd = lwd, 
              lty = lty)
        rect(at[i] - radj*boxwidth/2, 
             q1[i], 
             at[i] + ladj*boxwidth/2, 
             q3[i], col = rectCol)
        # median line segment
        lines(x = c(at[i] - radj*med.dens[i], 
                    at[i], 
                    at[i] + ladj*med.dens[i]),
              y = rep(med[i],3))
      }
    }
  }
  else {
    if (!add) {
      plot.window(xlim = ylim, ylim = xlim)
      axis(1)
      axis(2, at = at, label = label)
    }
    box()
    for (i in 1:n) {
      polygon(c(base[[i]], rev(base[[i]])), 
              c(at[i] - radj*height[[i]], rev(at[i] + ladj*height[[i]])), 
              col = col, border = border, 
              lty = lty, lwd = lwd)
      if (drawRect) {
        lines(c(lower[i], upper[i]), at[c(i, i)], lwd = lwd, 
              lty = lty)
        rect(q1[i], at[i] - radj*boxwidth/2, q3[i], at[i] + 
               ladj*boxwidth/2, col = rectCol)
        lines(y = c(at[i] - radj*med.dens[i], 
                    at[i], 
                    at[i] + ladj*med.dens[i]),
              x = rep(med[i],3))
      }
    }
  }
  invisible(list(upper = upper, lower = lower, median = med, 
                 q1 = q1, q3 = q3))
}

########## GeomSplitViolin ############
GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}
####### 3.c ##########
ggplot(pe, aes(x=Test, y=Abserror)) + 
  geom_boxplot(aes(fill=Test))  +
  geom_point(size=0.5,color='blue') +
  theme(legend.position='none')+
  theme(text = element_text(size=10),axis.text.x = element_text(angle=30, hjust=1)) +
  labs(title = "AbosulteError of Responses")+
  scale_x_discrete(labels=abr)


####### 3.d ##########
head(pe)
#line 1 for x,y aexs
#line2 for violin option
#line3 for postion
#line4 for text correction
#line6 for boxpot

ggplot(pe, aes(x=reorder(Test, Abserror, median), y=Abserror, fill=factor(Test))) + 
  geom_violin(size=1) + 
  theme(legend.position='none')+
  theme(text = element_text(size=10),axis.text.x = element_text(angle=30, hjust=1)) +
  labs(title = "AbosulteError of Responses")+
  geom_jitter(color="red", alpha=.3, size=3, width=.2) +
  theme(plot.title = element_text(hjust = 0.5,color="#666666", face="bold", size=15))+
  geom_boxplot(width=.1) + 
  scale_fill_manual(values=c("blue", "yellow", "red", "green", "lightblue", "purple", "pink", "grey"))
  
ggplot(pe, aes(x=reorder(Test,Abserror,median), y=Abserror,color=Test)) +
  geom_split_violin(size = 1) 

####### 3.d.2 ##########
ggplot(pe, aes(x=reorder(Test,error,median), y=error,color=Test)) +
  geom_violin(size = 1) +
  geom_boxplot(width=0.1,color="darkblue")+
  theme(legend.position='none')+
  theme(text = element_text(size=10),axis.text.x = element_text(angle=30, hjust=1)) +
  labs(title = "Underestimated or Overestimated of Test")+
  theme(plot.title = element_text(hjust = 0.5,color="#666666", face="bold", size=15))+
  scale_x_discrete(labels=abr) +
  xlab("Test")

####### 3.e ##########
# in this case, the display is the var with two different levels;
Subjects=pe[pe$Subject >=56 & pe$Subject<=73,]
Subjects$Display[Subjects$Display== 1] <- "Display1"
Subjects$Display[Subjects$Display== 2] <- "Display2"
ggplot(Subjects, aes(x=Test, y=error,color=Display))+
  geom_boxplot(width=0.05,color="darkblue")+
  geom_violin (alpha = 0.7, width =0.5) +
  #geom_split_violin(alpha = 0.7)
  theme(plot.title = element_text(hjust = 0.5,color="#666666", face="bold", size=15))+
  labs(title = "Response Pattern")

####### 3.f ##########
## subset the original dataset
pe$Display[pe$Display== 1] <- "Display1"
pe$Display[pe$Display== 2] <- "Display2"

data= subset(pe, Test=='Vertical Distance, Non-Aligned')
data1=data[data$Subject %in% c(54,56:65,67,68,71,73) & data$Test=='Vertical Distance, Non-Aligned' & data$Display=='Display1'& data$Response==1,]

## highlight abnormal points
ggplot(data1,aes(x=Subject, y=Response)) +
  geom_point(data=data,aes(x=Subject, y=Response ), color='darkblue',size=1) +
  geom_point(size=1.5,color='orange')+
  labs(title = "Find abnormal points") +
  theme(plot.title = element_text(hjust = 0.5,color="#666666", face="bold", size=15)) +
  theme(legend.position='none') +
  ylab(" Response Value")

####### 3.g ##########
##Subject 56-73
ggplot(Subjects, aes(x=Trial, y=Abserror,color=Display))+
  geom_boxplot(width=0.05,color="darkblue")+
  geom_split_violin(alpha = 0.7)+
  theme(plot.title = element_text(hjust = 0.5,color="#666666", face="bold", size=15))+
  labs(title = "Response Pattern")
#Subjects=student[student$Subject >=56 & student$Subject<=73,]
#Subjects$Display[Subjects$Display== 1] <- "Display1"
#Subjects$Display[Subjects$Display== 2] <- "Display2"

pe$Display[Subjects$Display== 1] <- "Display1"
pe$Display[Subjects$Display== 2] <- "Display2"
## All Subject 
ggplot(pe, aes(x=Trial, y=Abserror,color=Display))+
  geom_boxplot(width=0.05,color="darkblue")+
  geom_split_violin(alpha = 0.7)+
  theme(plot.title = element_text(hjust = 0.5,color="#666666", face="bold", size=15))+
  labs(title = "Response Pattern")


