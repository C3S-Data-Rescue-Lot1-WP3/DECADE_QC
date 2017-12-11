##### DEFINE QUALITY CONTROL TESTS FOR DETECTING SYSTEMATIC DATA QULAITY ISSUES #####


# For which atmospheric parameters should the test be applied?
# maximum temperature = TX, minimum temperature = TN, precipitation = PRCP
# Yes = "Y", no = "N"
param.TX <- "Y"
param.TN <- "Y"
param.PRCP <- "Y"

# Which output files should be created?
# Yes = "Y", no = "N"
point.plots <- "Y"
point.plots.threshold <- "Y" # set a maximum PRCP threshold for additional pointplots (PRCP only)
threshold.point.plots.PRCP <- 25 # define a maximum threshold for additional PRCP pointplots
decimal.plots <- "Y"
missing.values.plots <- "Y"
weeklycycles.complete.plots <- "Y"
weeklycycles.annual.plots <- "Y"
significance.level.weeklycycles <- 5 # significance level of the binominal test for weekly precipitation cycles in %

# Directories of the folders containing the station data in the DECADE file format
dir.TX <- "D:/DECADE/QC_homog_data/data/raw_data/Base_Concatenada_v5_level2/TX"
dir.TN <- "D:/DECADE/QC_homog_data/data/raw_data/Base_Concatenada_v5_level2/TN"
dir.PRCP <- "D:/DECADE/QC_homog_data/data/raw_data/Base_Concatenada_v5_level2/PRCP"

# Directories to save files
dir.save <- "D:/DECADE/QC_homog_data/QC/generalized"

# Define threshold for the minimum number of daily observations
station.days <- 7300

# Define threshold for outliers to be removed from the data (minimum and maximum acceptable TX, TN, and PRCP values)
threshold.temperature.max <- 70
threshold.temperature.min <- -70
threshold.PRCP.max <- 3000
threshold.PRCP.min <- 0

# R packages needed
library(reshape2)
library(ggplot2)

# Define parameters for visualization
max.segment.length.point.plots <- 30 # define maximum of years shown in each plot
pdf.width.point.plots <- 12
pdf.height.point.plots <- 8
point.size <- 0.3 # define point size of each single observation
max.segment.length.decimal.plots <- 30 # define maximum of years shown in each plot
pdf.width.decimal.plots <- 12
pdf.height.decimal.plots <- 7
max.segment.length.missing.values.plots <- 30 # define maximum of years shown in each plot
pdf.width.missing.values.plots <- 12
pdf.height.missing.values.plots <- 7
size.number.missing.values <- 4 # define the size of the number above the bar of annual missing values
pdf.width.weeklycycles.complete.plots <- 7
pdf.height.weeklycycles.complete.plots <- 5

# Nunber of header-lines to be skipped
skip.lines <- 14



#---------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------
# Create folders
#---------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------

if (param.TX == "Y"){
  setwd(dir.save)
  dir.create("TX")
  setwd(paste0(dir.save, "/TX"))
  if (point.plots == "Y"){
    dir.create("point_plots")
  }
  if (decimal.plots == "Y"){
    dir.create("decimal_plots")
  }
  if (missing.values.plots == "Y"){
    dir.create("missing_values_plots")
  }
}

if (param.TN == "Y"){
  setwd(dir.save)
  dir.create("TN")
  setwd(paste0(dir.save, "/TN"))
  if (point.plots == "Y"){
    dir.create("point_plots")
  }
  if (decimal.plots == "Y"){
    dir.create("decimal_plots")
  }
  if (missing.values.plots == "Y"){
    dir.create("missing_values_plots")
  }
}

if (param.PRCP == "Y"){
  setwd(dir.save)
  dir.create("PRCP")
  setwd(paste0(dir.save, "/PRCP"))
  if (point.plots == "Y"){
    dir.create("point_plots")
  }
  if (point.plots.threshold == "Y"){
    dir.create("point_plots_threshold")
  }
  if (decimal.plots == "Y"){
    dir.create("decimal_plots")
  }
  if (missing.values.plots == "Y"){
    dir.create("missing_values_plots")
  }
  if (weeklycycles.complete.plots == "Y"){
    dir.create("weekly_cycles_plots")
  }
  if (weeklycycles.annual.plots == "Y"){
    dir.create("weekly_cycles_annual_plots")
  }
}

# Define confidence interval thersholds as fraction
ci.val <- 1-significance.level.weeklycycles/100


#---------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------
# Temperature
#---------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------

T.variable <- c("TX", "TN")
analyze.T <- c(param.TX, param.TN)
for(set.param in 1:length(T.variable)){
  
  if (analyze.T[set.param] == "Y"){
    
    setwd(get(paste0("dir.", T.variable[set.param])))
    dailydata.files <- list.files()
    
    #Repeat the srcript for all files in the folder
    for(filenumber in 1:length(dailydata.files)){
      #Read the station name out of the filename
      filename <- dailydata.files[filenumber]
      station.name=paste0(strsplit(filename, "\\.dat"))
      
      #Read the file as a table
      tab.dailydata <- read.table(filename,skip=skip.lines)
      tab.dailydata[,5][tab.dailydata[,5] == -999.9] <- NA
      tab.dailydata[,5][tab.dailydata[,5] < threshold.temperature.min] <- NA
      tab.dailydata[,5][tab.dailydata[,5] > threshold.temperature.max] <- NA
      
      #Count days with values no NA
      if (length(which(!is.na(tab.dailydata[,5]))) < station.days) {
        
        print(paste0(T.variable[set.param], " ", station.name, ": too few observations"))
        
      } else {
        
        print(paste0(T.variable[set.param]," ",station.name))
        
        #################################################################################
        ### point plots
        #################################################################################
        
        if(point.plots == "Y"){
          
          abline_h <- as.numeric(round(min(tab.dailydata[,5], na.rm = T)):round(max(tab.dailydata[,5], na.rm = T)))
          abline_v <- paste0((min(tab.dailydata[,2], na.rm = T)) : (max(tab.dailydata[,2], na.rm = T)+1),"-01-01")
          
          # get segment lengths and pdf dimensions as defined in header
          segment.amounts <- length(min(tab.dailydata[,2]):max(tab.dailydata[,2]))/max.segment.length.point.plots
          segment.amounts.rounded <- ceiling(length(min(tab.dailydata[,2]):max(tab.dailydata[,2]))/max.segment.length.point.plots)
          start.year <- min(tab.dailydata[,2])
          for (k in 1:segment.amounts.rounded){
            seg <- tab.dailydata[which(tab.dailydata[,2] > start.year-1 & tab.dailydata[,2] < start.year+max.segment.length.point.plots),]
            
            # fill segment to length of max.segment.length.point.plots
            if (length(min(seg[,2]):max(seg[,2])) < max.segment.length.point.plots){
              add.dates <- as.Date(seq(ISOdate(max(seg[,2])+1,1,1), ISOdate(max(seg[,2])+max.segment.length.point.plots-length(min(seg[,2]):max(seg[,2])),12,31), "days"))
              add.matrix <- matrix(NA, nrow=length(add.dates), ncol=ncol(seg))
              add.matrix[,2] <- substring(add.dates,1,4)
              add.matrix[,3] <- substring(add.dates,6,7)
              add.matrix[,4] <- substring(add.dates,9,10)
              seg <- rbind(seg, add.matrix)
            }
            
            # get matrix with missing values
            seg.missing <- seg
            seg.missing[,5] <- ifelse(is.na(seg[,5]), min(tab.dailydata[,5], na.rm=T)-1, NA)
            seg.missing[which(as.numeric(seg.missing[,2]) > max(tab.dailydata[,2])), 5] <- NA
            
            # name segment
            assign(paste0("segment.no.", k), seg)
            assign(paste0("pdf.width.segment.no.", k), pdf.width.point.plots*(length(start.year:min(start.year+max.segment.length.point.plots-1, max(tab.dailydata[,2])))/max.segment.length.point.plots))
            assign(paste0("segment.missing.no.", k), seg.missing)
            start.year <- start.year+max.segment.length.point.plots
          }
          
          # plot single or multiple time series segements in one pdf
          mypath=file.path(paste0(dir.save, "/", T.variable[set.param], "/point_plots/", station.name,".pdf"))
          pdf(file=mypath,width=pdf.width.point.plots, height=pdf.height.point.plots*segment.amounts.rounded, paper="special")
          par(mfrow=c(segment.amounts.rounded, 1), mar=c(3,5,4,2))           
          for (k in 1:segment.amounts.rounded){
            plot(as.Date(paste(get(paste0("segment.no.", k))[,2], get(paste0("segment.no.", k))[,3], get(paste0("segment.no.", k))[,4],sep="-")), as.numeric(get(paste0("segment.no.", k))[,5]),
                 panel.first = abline(h = abline_h, v = as.Date(abline_v),col = "lightgray"),
                 ylim = c(min(tab.dailydata[,5]-1, na.rm=T), max(tab.dailydata[,5], na.rm=T)),
                 xlab="", ylab=paste0(T.variable[set.param]," (°C)"), main=station.name, pch=20, cex=point.size, lwd=3, cex.lab=1.5, cex.axis=1.5, cex.main=2,cex.sub=1.5)
            points(as.Date(paste(get(paste0("segment.missing.no.", k))[,2], get(paste0("segment.missing.no.", k))[,3], get(paste0("segment.missing.no.", k))[,4],sep="-")), as.numeric(get(paste0("segment.missing.no.", k))[,5]),
                   pch=4, cex=1.5, col="red")
          }
          graphics.off()
        }
        
        #################################################################################
        ### decimal plots
        #################################################################################
        
        if(decimal.plots == "Y"){
          
          #########
          # Multiple plot function
          #
          # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
          # - cols:   Number of columns in layout
          # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
          #
          # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
          # then plot 1 will go in the upper left, 2 will go in the upper right, and
          # 3 will go all the way across the bottom.
          #
          multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
            library(grid)
            
            # Make a list from the ... arguments and plotlist
            plots <- c(list(...), plotlist)
            
            numPlots = length(plots)
            
            # If layout is NULL, then use 'cols' to determine layout
            if (is.null(layout)) {
              # Make the panel
              # ncol: Number of columns of plots
              # nrow: Number of rows needed, calculated from # of cols
              layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                               ncol = cols, nrow = ceiling(numPlots/cols))
            }
            
            if (numPlots==1) {
              print(plots[[1]])
              
            } else {
              # Set up the page
              grid.newpage()
              pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
              
              # Make each plot, in the correct location
              for (i in 1:numPlots) {
                # Get the i,j matrix positions of the regions that contain this subplot
                matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                
                print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                layout.pos.col = matchidx$col))
              }
            }
          }
          #######
          
          # get decimals in precision of 0.1
          data.vec.na <- round(tab.dailydata[,5], digits=1)
          decimal.vec <- data.vec.na*NA #create dummy vector
          data.vec <- format(data.vec.na, nsmall=1)
          for (i in 1:length(data.vec)){
            decimal <- as.numeric(unlist(strsplit(format(data.vec[i]), ".", fixed = TRUE)))[2] /10
            decimal.vec[i] <- decimal
          }
          # date frame with date and decimals
          decimal.frame <- data.frame(tab.dailydata[,2], tab.dailydata[,3], tab.dailydata[,4], decimal.vec)
          
          # count decimals per year
          start <- min(decimal.frame[,1])
          end <- max(decimal.frame[,1])
          year.vec <- start:end
          count.vec <- start:end *NA
          decimal.frame.count <- data.frame(matrix(rep(year.vec,11), ncol=11))
          for(year in start:end){
            for(i in 0:9){
              declength <- length(which(decimal.frame[which(decimal.frame[,1]==year),4]==i/10))
              decimal.frame.count[year-start+1,i+2] <- declength
              colnames(decimal.frame.count) <- c("year","x.0","x.1","x.2","x.3","x.4","x.5","x.6","x.7","x.8","x.9")
            }
          }
          # replace rows (i.e. years) without values by NAs
          for(i in 1:length(year.vec)){
            if (sum(decimal.frame.count[i,2:11]) <=0) {
              decimal.frame.count[i,2:11] <- NA
            }
          }
          
          # get maximum number of annual decimals
          max.no.decimals <- max(rowSums(decimal.frame.count[,2:11]), na.rm=T)
          
          # get segment lengths and pdf dimensions as defined in header
          segment.amounts <- length(min(tab.dailydata[,2]):max(tab.dailydata[,2]))/max.segment.length.decimal.plots
          segment.amounts.rounded <- ceiling(length(min(tab.dailydata[,2]):max(tab.dailydata[,2]))/max.segment.length.decimal.plots)
          start.year <- min(tab.dailydata[,2])
          for (k in 1:segment.amounts.rounded){
            seg <- decimal.frame.count[which(decimal.frame.count[,1] > start.year-1 & decimal.frame.count[,1] < start.year+max.segment.length.decimal.plots),]
            
            # fill segment to length of max.segment.length.decimal.plots
            if (length(min(seg[,1]):max(seg[,1])) < max.segment.length.decimal.plots){
              add.dates <- max.segment.length.decimal.plots-length(min(seg[,1]):max(seg[,1]))
              add.matrix <- matrix(NA, nrow=add.dates, ncol=ncol(seg))
              add.matrix[,1] <- (max(seg[,1])+1):(start.year+max.segment.length.decimal.plots-1)
              colnames(add.matrix) <- colnames(seg)
              seg <- rbind(seg, add.matrix)
            }
            
            # name segment
            assign(paste0("segment.no.", k), seg)
            assign(paste0("pdf.width.segment.no.", k), pdf.width.decimal.plots*(length(start.year:min(start.year+max.segment.length.decimal.plots-1, max(tab.dailydata[,2])))/max.segment.length.decimal.plots))
            assign(paste0("segment.missing.no.", k), seg.missing)
            start.year <- start.year+max.segment.length.decimal.plots
          }
          
          # plot single or multiple time series segements in one pdf
          mypath=file.path(paste0(dir.save, "/",T.variable[set.param],"/decimal_plots/", station.name,".pdf"))
          pdf(file=mypath,width=pdf.width.decimal.plots, height=pdf.height.decimal.plots*segment.amounts.rounded, paper="special", onefile=TRUE)
          plots <- list()  # new empty list
          for (k in 1:segment.amounts.rounded){
            decimal.frame.count <- melt(get(paste0("segment.no.", k)), id.vars='year')
            rhg_cols <- c("black", "yellow", "orange", "red", "darkslateblue", "darkgray", "magenta","blue", "cyan", "darkgreen")
            segment.plot = (ggplot(decimal.frame.count, aes(x=year, y=value, fill=variable))
                            + geom_bar(stat='identity', width=.7) + scale_fill_manual(values = rhg_cols)
                            + xlim(min(decimal.frame.count[,1])-0.5, max(decimal.frame.count[,1])+0.5)
                            + ylim(0, max.no.decimals)
                            + theme(plot.title=element_text(size=22, face="bold"), axis.text=element_text(size=18) , axis.title=element_text(size=18, colour="black",face=NULL), axis.title.y = element_text(margin = margin(r = 15)), axis.title.x = element_blank(), legend.title=element_blank(), legend.text = element_text(colour="black", size=15), axis.title.y=element_text(vjust=1.3), plot.margin = unit(c(0.6,0.2,0.5,0.5), "cm"))
                            + guides(fill = guide_legend(reverse=TRUE)) + ylab("frequency (days/year)")
                            + ggtitle(paste(station.name, "\n")))
            plots[[k]] <- segment.plot
          }
          multiplot(plotlist = plots)
          graphics.off()
        }
        
        #################################################################################
        ### missing values plots
        #################################################################################
        
        if(missing.values.plots == "Y"){
          
          #########
          # Multiple plot function
          #
          # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
          # - cols:   Number of columns in layout
          # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
          #
          # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
          # then plot 1 will go in the upper left, 2 will go in the upper right, and
          # 3 will go all the way across the bottom.
          #
          multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
            library(grid)
            
            # Make a list from the ... arguments and plotlist
            plots <- c(list(...), plotlist)
            
            numPlots = length(plots)
            
            # If layout is NULL, then use 'cols' to determine layout
            if (is.null(layout)) {
              # Make the panel
              # ncol: Number of columns of plots
              # nrow: Number of rows needed, calculated from # of cols
              layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                               ncol = cols, nrow = ceiling(numPlots/cols))
            }
            
            if (numPlots==1) {
              print(plots[[1]])
              
            } else {
              # Set up the page
              grid.newpage()
              pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
              
              # Make each plot, in the correct location
              for (i in 1:numPlots) {
                # Get the i,j matrix positions of the regions that contain this subplot
                matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                
                print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                layout.pos.col = matchidx$col))
              }
            }
          }
          #######
          
          missing.frame <- data.frame(tab.dailydata[,2], tab.dailydata[,5])
          
          # count missing values per year
          start <- min(missing.frame[,1])
          end <- max(missing.frame[,1])
          year.vec <- start:end
          count.vec <- start:end *NA
          missing.frame.count <- data.frame(matrix(year.vec), ncol=3)
          missing.frame.count[,2] <- "missing"
          missing.frame.count[,3] <- NA
          for(year in start:end){
            missing.length <- sum(is.na(missing.frame[which(missing.frame[,1]==year),2]))
            missing.frame.count[year-start+1,3] <- missing.length
            colnames(missing.frame.count) <- c("year","variable", "value")
          }
          
          # get segment lengths and pdf dimensions as defined in header
          segment.amounts <- length(min(tab.dailydata[,2]):max(tab.dailydata[,2]))/max.segment.length.missing.values.plots
          segment.amounts.rounded <- ceiling(length(min(tab.dailydata[,2]):max(tab.dailydata[,2]))/max.segment.length.missing.values.plots)
          start.year <- min(tab.dailydata[,2])
          for (k in 1:segment.amounts.rounded){
            seg <- missing.frame.count[which(missing.frame.count[,1] > start.year-1 & missing.frame.count[,1] < start.year+max.segment.length.missing.values.plots),]
            
            # fill segment to length of max.segment.length.missing.values.plots
            if (length(min(seg[,1]):max(seg[,1])) < max.segment.length.missing.values.plots){
              add.dates <- max.segment.length.missing.values.plots-length(min(seg[,1]):max(seg[,1]))
              add.matrix <- matrix(NA, nrow=add.dates, ncol=ncol(seg))
              add.matrix[,1] <- (max(seg[,1])+1):(start.year+max.segment.length.missing.values.plots-1)
              colnames(add.matrix) <- colnames(seg)
              seg <- rbind(seg, add.matrix)
            }
            
            # name segment
            assign(paste0("segment.no.", k), seg)
            assign(paste0("pdf.width.segment.no.", k), pdf.width.missing.values.plots*(length(start.year:min(start.year+max.segment.length.missing.values.plots-1, max(tab.dailydata[,2])))/max.segment.length.missing.values.plots))
            assign(paste0("segment.missing.no.", k), seg)
            start.year <- start.year+max.segment.length.missing.values.plots
          }
          
          # plot single or multiple time series segements in one pdf
          mypath=file.path(paste0(dir.save, "/",T.variable[set.param],"/missing_values_plots/", station.name,".pdf"))
          pdf(file=mypath,width=pdf.width.missing.values.plots, height=pdf.height.missing.values.plots*segment.amounts.rounded, paper="special", onefile=TRUE)
          plots <- list()  # new empty list
          for (k in 1:segment.amounts.rounded){
            missing.values.frame.count <- get(paste0("segment.no.", k))
            rhg_cols <- c("black")
            segment.plot = (ggplot(missing.values.frame.count, aes(x=year, y=value, fill=variable))
                            + geom_bar(stat='identity', width=.7)
                            + geom_text(aes(label=value), vjust = -.5, size = size.number.missing.values)
                            + ylim(0, 366)
                            + scale_fill_manual(values = rhg_cols)
                            + theme(plot.title=element_text(size=22, face="bold"), axis.text=element_text(size=18) , axis.title=element_text(size=18, colour="black",face=NULL), axis.title.y = element_text(margin = margin(r = 15)), axis.title.x = element_blank(), legend.title=element_blank(), legend.text = element_blank(), legend.position="none", axis.title.y=element_text(vjust=1.5), plot.margin = unit(c(0.6,0.5,0.6,0.5), "cm"))
                            + guides(fill = guide_legend(reverse=TRUE)) + ylab("missing values / year")
                            + ggtitle(paste(station.name, "\n")))
            plots[[k]] <- segment.plot
          }
          multiplot(plotlist = plots)
          graphics.off()
        }
      }
    }
  }
}


#---------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------
# PRCP
#---------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------

if (param.PRCP == "Y"){
  
  setwd(dir.PRCP)
  dailydata.files<-list.files()
  
  #Repeat the srcript for all files in the folder
  for(filenumber in 1:length(dailydata.files)){
    #Read the station name out of the filename
    filename <- dailydata.files[filenumber]
    station.name=paste0(strsplit(filename, "\\.dat"))
    
    #Read the file as a table
    tab.dailydata <- read.table(filename,skip=skip.lines)
    tab.dailydata[,5][tab.dailydata[,5] == -999.9] <- NA
    tab.dailydata[,5][tab.dailydata[,5] < threshold.PRCP.min] <- NA
    tab.dailydata[,5][tab.dailydata[,5] > threshold.PRCP.max] <- NA
    
    #Count days with values no NA
    if (length(which(!is.na(tab.dailydata[,5]))) < station.days) {
      print(paste0("PRCP ", station.name, ": too few observations"))
      
    } else {
      
      print(paste0("PRCP ",station.name))
      
      #################################################################################
      ### point plots
      #################################################################################
      
      if(point.plots == "Y"){
        
        abline_h <- as.numeric(round(min(tab.dailydata[,5], na.rm = T)):(round(max(tab.dailydata[,5], na.rm = T))+1))
        abline_v <- paste0((min(tab.dailydata[,2], na.rm = T)) : (max(tab.dailydata[,2], na.rm = T)+1),"-01-01")
        
        # get segment lengths and pdf dimensions as defined in header
        segment.amounts <- length(min(tab.dailydata[,2]):max(tab.dailydata[,2]))/max.segment.length.point.plots
        segment.amounts.rounded <- ceiling(length(min(tab.dailydata[,2]):max(tab.dailydata[,2]))/max.segment.length.point.plots)
        start.year <- min(tab.dailydata[,2])
        for (k in 1:segment.amounts.rounded){
          seg <- tab.dailydata[which(tab.dailydata[,2] > start.year-1 & tab.dailydata[,2] < start.year+max.segment.length.point.plots),]
          
          # fill segment to length of max.segment.length.point.plots
          if (length(min(seg[,2]):max(seg[,2])) < max.segment.length.point.plots){
            add.dates <- as.Date(seq(ISOdate(max(seg[,2])+1,1,1), ISOdate(max(seg[,2])+max.segment.length.point.plots-length(min(seg[,2]):max(seg[,2])),12,31), "days"))
            add.matrix <- matrix(NA, nrow=length(add.dates), ncol=ncol(seg))
            add.matrix[,2] <- substring(add.dates,1,4)
            add.matrix[,3] <- substring(add.dates,6,7)
            add.matrix[,4] <- substring(add.dates,9,10)
            seg <- rbind(seg, add.matrix)
          }
          
          # get matrix with missing values
          seg.missing <- seg
          seg.missing[,5] <- ifelse(is.na(seg[,5]), min(tab.dailydata[,5], na.rm=T)-(max(tab.dailydata[,5], na.rm=T)/100*3), NA)
          seg.missing[which(as.numeric(seg.missing[,2]) > max(tab.dailydata[,2])), 5] <- NA
          
          # name segment
          assign(paste0("segment.no.", k), seg)
          assign(paste0("pdf.width.segment.no.", k), pdf.width.point.plots*(length(start.year:min(start.year+max.segment.length.point.plots-1, max(tab.dailydata[,2])))/max.segment.length.point.plots))
          assign(paste0("segment.missing.no.", k), seg.missing)
          start.year <- start.year+max.segment.length.point.plots
        }
        
        # plot single or multiple time series segements in one pdf
        mypath=file.path(paste0(dir.save, "/PRCP/point_plots/", station.name,".pdf"))
        pdf(file=mypath,width=pdf.width.point.plots, height=pdf.height.point.plots*segment.amounts.rounded, paper="special")
        par(mfrow=c(segment.amounts.rounded, 1), mar=c(3,5,4,2))           
        for (k in 1:segment.amounts.rounded){
          plot(as.Date(paste(get(paste0("segment.no.", k))[,2], get(paste0("segment.no.", k))[,3], get(paste0("segment.no.", k))[,4],sep="-")), as.numeric(get(paste0("segment.no.", k))[,5]),
               panel.first = abline(h = abline_h, v = as.Date(abline_v),col = "lightgray"),
               ylim = c(min(tab.dailydata[,5]-1, na.rm=T), max(tab.dailydata[,5], na.rm=T)),
               xlab="", ylab=paste0("PRCP (mm)"), main=station.name, pch=20, cex=point.size, lwd=3, cex.lab=1.5, cex.axis=1.5, cex.main=2,cex.sub=1.5)
          abline(h=seq(0, max(tab.dailydata[,5], na.rm=TRUE), 10), col="lightgray", lwd = 2)
          points(as.Date(paste(get(paste0("segment.no.", k))[,2], get(paste0("segment.no.", k))[,3], get(paste0("segment.no.", k))[,4],sep="-")), as.numeric(get(paste0("segment.no.", k))[,5]), pch=20, cex=point.size, lwd=3, cex.lab=1.5, cex.axis=1.5, cex.main=2,cex.sub=1.5)
          points(as.Date(paste(get(paste0("segment.missing.no.", k))[,2], get(paste0("segment.missing.no.", k))[,3], get(paste0("segment.missing.no.", k))[,4],sep="-")), as.numeric(get(paste0("segment.missing.no.", k))[,5]),
                 pch=4, cex=1.5, col="red")
        }
        graphics.off()
      }
      
      #################################################################################
      ### point plots with a threshold
      #################################################################################
      
      if(point.plots.threshold == "Y"){
        
        # remove values above threshold
        tab.dailydata.thr <- tab.dailydata[,5]
        tab.dailydata.thr[tab.dailydata.thr > threshold.point.plots.PRCP] <- NA
        
        abline_h <- as.numeric(round(min(tab.dailydata.thr, na.rm = T)):(round(max(tab.dailydata.thr, na.rm = T))+1))
        abline_v <- paste0((min(tab.dailydata[,2], na.rm = T)) : (max(tab.dailydata[,2], na.rm = T)+1),"-01-01")
        
        # get segment lengths and pdf dimensions as defined in header
        segment.amounts <- length(min(tab.dailydata[,2]):max(tab.dailydata[,2]))/max.segment.length.point.plots
        segment.amounts.rounded <- ceiling(length(min(tab.dailydata[,2]):max(tab.dailydata[,2]))/max.segment.length.point.plots)
        start.year <- min(tab.dailydata[,2])
        for (k in 1:segment.amounts.rounded){
          seg <- tab.dailydata[which(tab.dailydata[,2] > start.year-1 & tab.dailydata[,2] < start.year+max.segment.length.point.plots),]
          
          # fill segment to length of max.segment.length.point.plots
          if (length(min(seg[,2]):max(seg[,2])) < max.segment.length.point.plots){
            add.dates <- as.Date(seq(ISOdate(max(seg[,2])+1,1,1), ISOdate(max(seg[,2])+max.segment.length.point.plots-length(min(seg[,2]):max(seg[,2])),12,31), "days"))
            add.matrix <- matrix(NA, nrow=length(add.dates), ncol=ncol(seg))
            add.matrix[,2] <- substring(add.dates,1,4)
            add.matrix[,3] <- substring(add.dates,6,7)
            add.matrix[,4] <- substring(add.dates,9,10)
            seg <- rbind(seg, add.matrix)
          }
          
          # get matrix with missing values
          seg.missing <- seg
          seg.missing[,5] <- ifelse(is.na(seg[,5]), min(tab.dailydata.thr, na.rm=T)-(max(tab.dailydata.thr, na.rm=T)/100*3), NA)
          seg.missing[which(as.numeric(seg.missing[,2]) > max(tab.dailydata[,2])), 5] <- NA
          
          # name segment
          assign(paste0("segment.no.", k), seg)
          assign(paste0("pdf.width.segment.no.", k), pdf.width.point.plots*(length(start.year:min(start.year+max.segment.length.point.plots-1, max(tab.dailydata[,2])))/max.segment.length.point.plots))
          assign(paste0("segment.missing.no.", k), seg.missing)
          start.year <- start.year+max.segment.length.point.plots
        }
        
        # plot single or multiple time series segements in one pdf
        mypath=file.path(paste0(dir.save, "/PRCP/point_plots_threshold/", station.name,".pdf"))
        pdf(file=mypath,width=pdf.width.point.plots, height=pdf.height.point.plots*segment.amounts.rounded, paper="special")
        par(mfrow=c(segment.amounts.rounded, 1), mar=c(3,5,4,2))           
        for (k in 1:segment.amounts.rounded){
          plot(as.Date(paste(get(paste0("segment.no.", k))[,2], get(paste0("segment.no.", k))[,3], get(paste0("segment.no.", k))[,4],sep="-")), as.numeric(get(paste0("segment.no.", k))[,5]),
               panel.first = abline(h = abline_h, v = as.Date(abline_v),col = "lightgray"),
               ylim = c(min(tab.dailydata.thr-1, na.rm=T), max(tab.dailydata.thr, na.rm=T)),
               xlab="", ylab=paste0("PRCP (mm)"), main=station.name, pch=20, cex=point.size, lwd=3, cex.lab=1.5, cex.axis=1.5, cex.main=2,cex.sub=1.5)
          abline(h=seq(0, max(tab.dailydata.thr, na.rm=TRUE), 10), col="lightgray", lwd = 2)
          points(as.Date(paste(get(paste0("segment.no.", k))[,2], get(paste0("segment.no.", k))[,3], get(paste0("segment.no.", k))[,4],sep="-")), as.numeric(get(paste0("segment.no.", k))[,5]), pch=20, cex=point.size, lwd=3, cex.lab=1.5, cex.axis=1.5, cex.main=2,cex.sub=1.5)
          points(as.Date(paste(get(paste0("segment.missing.no.", k))[,2], get(paste0("segment.missing.no.", k))[,3], get(paste0("segment.missing.no.", k))[,4],sep="-")), as.numeric(get(paste0("segment.missing.no.", k))[,5]),
                 pch=4, cex=1.5, col="red")
        }
        graphics.off()
      }
      
      #################################################################################
      ### decimal plots
      #################################################################################
      
      if(decimal.plots == "Y"){
        
        #########
        # Multiple plot function
        #
        # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
        # - cols:   Number of columns in layout
        # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
        #
        # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
        # then plot 1 will go in the upper left, 2 will go in the upper right, and
        # 3 will go all the way across the bottom.
        #
        multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
          library(grid)
          
          # Make a list from the ... arguments and plotlist
          plots <- c(list(...), plotlist)
          
          numPlots = length(plots)
          
          # If layout is NULL, then use 'cols' to determine layout
          if (is.null(layout)) {
            # Make the panel
            # ncol: Number of columns of plots
            # nrow: Number of rows needed, calculated from # of cols
            layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                             ncol = cols, nrow = ceiling(numPlots/cols))
          }
          
          if (numPlots==1) {
            print(plots[[1]])
            
          } else {
            # Set up the page
            grid.newpage()
            pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
            
            # Make each plot, in the correct location
            for (i in 1:numPlots) {
              # Get the i,j matrix positions of the regions that contain this subplot
              matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
              
              print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                              layout.pos.col = matchidx$col))
            }
          }
        }
        #######
        
        tab.dailydata.digits <- tab.dailydata[,5]
        tab.dailydata.digits[tab.dailydata.digits <= 0] <- NA
        
        # get decimals in precision of 0.1
        data.vec.na <- round(tab.dailydata.digits, digits=1)
        decimal.vec <- data.vec.na*NA #create dummy vector
        data.vec <- format(data.vec.na, nsmall=1)
        for (i in 1:length(data.vec)){
          decimal <- as.numeric(unlist(strsplit(format(data.vec[i]), ".", fixed = TRUE)))[2] /10
          decimal.vec[i] <- decimal
        }
        # date frame with date and decimals
        decimal.frame <- data.frame(tab.dailydata[,2], tab.dailydata[,3], tab.dailydata[,4], decimal.vec)
        
        # count decimals per year
        start <- min(decimal.frame[,1])
        end <- max(decimal.frame[,1])
        year.vec <- start:end
        count.vec <- start:end *NA
        decimal.frame.count <- data.frame(matrix(rep(year.vec,11), ncol=11))
        for(year in start:end){
          for(i in 0:9){
            declength <- length(which(decimal.frame[which(decimal.frame[,1]==year),4]==i/10))
            decimal.frame.count[year-start+1,i+2] <- declength
            colnames(decimal.frame.count) <- c("year","x.0","x.1","x.2","x.3","x.4","x.5","x.6","x.7","x.8","x.9")
          }
        }
        # replace rows (i.e. years) without values by NAs
        for(i in 1:length(year.vec)){
          if (sum(decimal.frame.count[i,2:11]) <=0) {
            decimal.frame.count[i,2:11] <- NA
          }
        }
        
        # get maximum number of annual decimals
        max.no.decimals <- max(rowSums(decimal.frame.count[,2:11]), na.rm=T)
        
        # get segment lengths and pdf dimensions as defined in header
        segment.amounts <- length(min(tab.dailydata[,2]):max(tab.dailydata[,2]))/max.segment.length.decimal.plots
        segment.amounts.rounded <- ceiling(length(min(tab.dailydata[,2]):max(tab.dailydata[,2]))/max.segment.length.decimal.plots)
        start.year <- min(tab.dailydata[,2])
        for (k in 1:segment.amounts.rounded){
          seg <- decimal.frame.count[which(decimal.frame.count[,1] > start.year-1 & decimal.frame.count[,1] < start.year+max.segment.length.decimal.plots),]
          
          # fill segment to length of max.segment.length.decimal.plots
          if (length(min(seg[,1]):max(seg[,1])) < max.segment.length.decimal.plots){
            add.dates <- max.segment.length.decimal.plots-length(min(seg[,1]):max(seg[,1]))
            add.matrix <- matrix(NA, nrow=add.dates, ncol=ncol(seg))
            add.matrix[,1] <- (max(seg[,1])+1):(start.year+max.segment.length.decimal.plots-1)
            colnames(add.matrix) <- colnames(seg)
            seg <- rbind(seg, add.matrix)
          }
          
          # name segment
          assign(paste0("segment.no.", k), seg)
          assign(paste0("pdf.width.segment.no.", k), pdf.width.decimal.plots*(length(start.year:min(start.year+max.segment.length.decimal.plots-1, max(tab.dailydata[,2])))/max.segment.length.decimal.plots))
          assign(paste0("segment.missing.no.", k), seg.missing)
          start.year <- start.year+max.segment.length.decimal.plots
        }
        
        # plot single or multiple time series segements in one pdf
        mypath=file.path(paste0(dir.save, "/PRCP/decimal_plots/", station.name,".pdf"))
        pdf(file=mypath,width=pdf.width.decimal.plots, height=pdf.height.decimal.plots*segment.amounts.rounded, paper="special", onefile=TRUE)
        plots <- list()  # new empty list
        for (k in 1:segment.amounts.rounded){
          decimal.frame.count <- melt(get(paste0("segment.no.", k)), id.vars='year')
          rhg_cols <- c("black", "yellow", "orange", "red", "darkslateblue", "darkgray", "magenta","blue", "cyan", "darkgreen")
          segment.plot = (ggplot(decimal.frame.count, aes(x=year, y=value, fill=variable))
                          + geom_bar(stat='identity', width=.7) + scale_fill_manual(values = rhg_cols)
                          + xlim(min(decimal.frame.count[,1])-0.5, max(decimal.frame.count[,1])+0.5)
                          + ylim(0, max.no.decimals)
                          + theme(plot.title=element_text(size=22, face="bold"), axis.text=element_text(size=18) , axis.title=element_text(size=18, colour="black",face=NULL), axis.title.y = element_text(margin = margin(r = 15)), axis.title.x = element_blank(), legend.title=element_blank(), legend.text = element_text(colour="black", size=15), axis.title.y=element_text(vjust=1.3), plot.margin = unit(c(0.6,0.2,0.5,0.5), "cm"))
                          + guides(fill = guide_legend(reverse=TRUE)) + ylab("frequency (days/year)")
                          + ggtitle(paste(station.name, "\n")))
          plots[[k]] <- segment.plot
        }
        multiplot(plotlist = plots)
        graphics.off()
      }
      
      #################################################################################
      ### missing values plots
      #################################################################################
      
      if(missing.values.plots == "Y"){
        
        #########
        # Multiple plot function
        #
        # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
        # - cols:   Number of columns in layout
        # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
        #
        # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
        # then plot 1 will go in the upper left, 2 will go in the upper right, and
        # 3 will go all the way across the bottom.
        #
        multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
          library(grid)
          
          # Make a list from the ... arguments and plotlist
          plots <- c(list(...), plotlist)
          
          numPlots = length(plots)
          
          # If layout is NULL, then use 'cols' to determine layout
          if (is.null(layout)) {
            # Make the panel
            # ncol: Number of columns of plots
            # nrow: Number of rows needed, calculated from # of cols
            layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                             ncol = cols, nrow = ceiling(numPlots/cols))
          }
          
          if (numPlots==1) {
            print(plots[[1]])
            
          } else {
            # Set up the page
            grid.newpage()
            pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
            
            # Make each plot, in the correct location
            for (i in 1:numPlots) {
              # Get the i,j matrix positions of the regions that contain this subplot
              matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
              
              print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                              layout.pos.col = matchidx$col))
            }
          }
        }
        #######
        
        missing.frame <- data.frame(tab.dailydata[,2], tab.dailydata[,5])
        
        # count missing values per year
        start <- min(missing.frame[,1])
        end <- max(missing.frame[,1])
        year.vec <- start:end
        count.vec <- start:end *NA
        missing.frame.count <- data.frame(matrix(year.vec), ncol=3)
        missing.frame.count[,2] <- "missing"
        missing.frame.count[,3] <- NA
        for(year in start:end){
          missing.length <- sum(is.na(missing.frame[which(missing.frame[,1]==year),2]))
          missing.frame.count[year-start+1,3] <- missing.length
          colnames(missing.frame.count) <- c("year","variable", "value")
        }
        
        # get segment lengths and pdf dimensions as defined in header
        segment.amounts <- length(min(tab.dailydata[,2]):max(tab.dailydata[,2]))/max.segment.length.missing.values.plots
        segment.amounts.rounded <- ceiling(length(min(tab.dailydata[,2]):max(tab.dailydata[,2]))/max.segment.length.missing.values.plots)
        start.year <- min(tab.dailydata[,2])
        for (k in 1:segment.amounts.rounded){
          seg <- missing.frame.count[which(missing.frame.count[,1] > start.year-1 & missing.frame.count[,1] < start.year+max.segment.length.missing.values.plots),]
          
          # fill segment to length of max.segment.length.missing.values.plots
          if (length(min(seg[,1]):max(seg[,1])) < max.segment.length.missing.values.plots){
            add.dates <- max.segment.length.missing.values.plots-length(min(seg[,1]):max(seg[,1]))
            add.matrix <- matrix(NA, nrow=add.dates, ncol=ncol(seg))
            add.matrix[,1] <- (max(seg[,1])+1):(start.year+max.segment.length.missing.values.plots-1)
            colnames(add.matrix) <- colnames(seg)
            seg <- rbind(seg, add.matrix)
          }
          
          # name segment
          assign(paste0("segment.no.", k), seg)
          assign(paste0("pdf.width.segment.no.", k), pdf.width.missing.values.plots*(length(start.year:min(start.year+max.segment.length.missing.values.plots-1, max(tab.dailydata[,2])))/max.segment.length.missing.values.plots))
          assign(paste0("segment.missing.no.", k), seg)
          start.year <- start.year+max.segment.length.missing.values.plots
        }
        
        # plot single or multiple time series segements in one pdf
        mypath=file.path(paste0(dir.save, "/PRCP/missing_values_plots/", station.name,".pdf"))
        pdf(file=mypath,width=pdf.width.missing.values.plots, height=pdf.height.missing.values.plots*segment.amounts.rounded, paper="special", onefile=TRUE)
        plots <- list()  # new empty list
        for (k in 1:segment.amounts.rounded){
          missing.values.frame.count <- get(paste0("segment.no.", k))
          rhg_cols <- c("black")
          segment.plot = (ggplot(missing.values.frame.count, aes(x=year, y=value, fill=variable))
                          + geom_bar(stat='identity', width=.7)
                          + geom_text(aes(label=value), vjust = -.5, size = size.number.missing.values)
                          + ylim(0, 366)
                          + scale_fill_manual(values = rhg_cols)
                          + theme(plot.title=element_text(size=22, face="bold"), axis.text=element_text(size=18) , axis.title=element_text(size=18, colour="black",face=NULL), axis.title.y = element_text(margin = margin(r = 15)), axis.title.x = element_blank(), legend.title=element_blank(), legend.text = element_blank(), legend.position="none", axis.title.y=element_text(vjust=1.5), plot.margin = unit(c(0.6,0.5,0.6,0.5), "cm"))
                          + guides(fill = guide_legend(reverse=TRUE)) + ylab("missing values / year")
                          + ggtitle(paste(station.name, "\n")))
          plots[[k]] <- segment.plot
        }
        multiplot(plotlist = plots)
        graphics.off()
      }
      
      #################################################################################
      ### weekly PRCP cycles (complete time series)
      #################################################################################
      
      if(weeklycycles.complete.plots == "Y"){
        
        # Convert the data into right format (yyyy-mm-dd to "weekday dd mm yyyy)
        datetmp <- paste(tab.dailydata[,4],".",tab.dailydata[,3],".",tab.dailydata[,2],sep="")
        rdate <- strftime((strptime(datetmp,format="%d. %m. %Y")),format="%a %d %m %Y")
        
        # create data.frame "dailydata" with to columns "rdate" and "ramount"
        ramount<-as.numeric(tab.dailydata[,5])
        dailydata<-data.frame(rdate,ramount)
        #Seperate the date into 3 single rows [yyyy,mm,dd]
        weekday<-substr(rdate,1,2)
        yr<-substr(dailydata[,1],10,14)
        m<-substr(dailydata[,1],7,8)
        d<-substr(dailydata[,1],4,5)
        
        # Years, Month, Days and "ramount" together in one data frame
        tab.data<-data.frame(yr,m,d,weekday,ramount)
        m<-as.numeric(m)
        
        wetdays<-ramount
        
        for (i in 1:length(ramount)){
          wetdays[i]<-ifelse(ramount[i]>=1,1,0)
        }
        
        raindays.count <- aggregate(wetdays,list(tab.data[,"weekday"]),FUN=sum,na.rm=T) # no. of wetdays per weekday
        alldays <- replace(wetdays, wetdays == 0, 1)
        weekdays.count <- aggregate(alldays,list(tab.data[,"weekday"]),FUN=sum,na.rm=T) # total no. of days per weekday
        
        mon_wd <- raindays.count[5,2]
        tue_wd <- raindays.count[1,2]
        wed_wd <- raindays.count[4,2]
        thu_wd <- raindays.count[2,2]
        fri_wd <- raindays.count[3,2]
        sat_wd <- raindays.count[6,2]
        sun_wd <- raindays.count[7,2]
        
        mon_all <- weekdays.count[5,2]
        tue_all <- weekdays.count[1,2]
        wed_all <- weekdays.count[4,2]
        thu_all <- weekdays.count[2,2]
        fri_all <- weekdays.count[3,2]
        sat_all <- weekdays.count[6,2]
        sun_all <- weekdays.count[7,2]
        
        wetall <- mon_wd+tue_wd+wed_wd+thu_wd+fri_wd+sat_wd+sun_wd
        daysall <- mon_all+tue_all+wed_all+thu_all+fri_all+sat_all+sun_all
        
        phi <- wetall/daysall # probability of a day to be a wd
        
        ci_mon <- binom.test(mon_wd, mon_all, p = phi, alternative ="two.sided", conf.level = ci.val) # binomail test if data in agreement with null hypothesis
        if (!(ci_mon$conf.int[1] < phi & phi < ci_mon$conf.int[2])){
          ciind_mon <- 1
        } else {
          ciind_mon <- 0
        }
        ci_tue <- binom.test(tue_wd, tue_all, p = phi, alternative ="two.sided", conf.level = ci.val) # binomail test if data in agreement with null hypothesis
        if (!(ci_tue$conf.int[1] < phi & phi < ci_tue$conf.int[2])){
          ciind_tue <- 2
        } else {
          ciind_tue <- 0
        }
        ci_wed <- binom.test(wed_wd, wed_all, p = phi, alternative ="two.sided", conf.level = ci.val) # binomail test if data in agreement with null hypothesis
        if (!(ci_wed$conf.int[1] < phi & phi < ci_wed$conf.int[2])){
          ciind_wed <- 3
        } else {
          ciind_wed <- 0
        }
        ci_thu <- binom.test(thu_wd, thu_all, p = phi, alternative ="two.sided", conf.level = ci.val) # binomail test if data in agreement with null hypothesis
        if (!(ci_thu$conf.int[1] < phi & phi < ci_thu$conf.int[2])){
          ciind_thu <- 4
        } else {
          ciind_thu <- 0
        }
        ci_fri <- binom.test(fri_wd, fri_all, p = phi, alternative ="two.sided", conf.level = ci.val) # binomail test if data in agreement with null hypothesis
        if (!(ci_fri$conf.int[1] < phi & phi < ci_fri$conf.int[2])){
          ciind_fri <- 5
        } else {
          ciind_fri <- 0
        }
        ci_sat <- binom.test(sat_wd, sat_all, p = phi, alternative ="two.sided", conf.level = ci.val) # binomail test if data in agreement with null hypothesis
        if (!(ci_sat$conf.int[1] < phi & phi < ci_sat$conf.int[2])){
          ciind_sat <- 6
        } else {
          ciind_sat <- 0
        }
        ci_sun <- binom.test(sun_wd, sun_all, p = phi, alternative ="two.sided", conf.level = ci.val) # binomail test if data in agreement with null hypothesis
        if (!(ci_sun$conf.int[1] < phi & phi < ci_sun$conf.int[2])){
          ciind_sun <- 7
        } else {
          ciind_sun <- 0
        }
        ci_out <- c(ciind_mon,ciind_tue,ciind_wed,ciind_thu,ciind_fri,ciind_sat,ciind_sun)
        
        mon_prob <- mon_wd/mon_all
        tue_prob <- tue_wd/tue_all
        wed_prob <- wed_wd/wed_all
        thu_prob <- thu_wd/thu_all
        fri_prob <- fri_wd/fri_all
        sat_prob <- sat_wd/sat_all
        sun_prob <- sun_wd/sun_all
        d.count <- c(mon_prob, tue_prob, wed_prob, thu_prob, fri_prob, sat_prob, sun_prob)
        w.day<-c(1:7)
        output<-data.frame(w.day,d.count)
        
        ### create and save plots
        pdf(file=paste0(dir.save, "/PRCP/weekly_cycles_plots/", station.name,".pdf"), width=pdf.width.weeklycycles.complete.plots, height=pdf.height.weeklycycles.complete.plots)
        par(mar=c(2.5,5,3,2))
        
        # create dummy-plot
        w.day<-c(1:7)
        day.names<-c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
        plot(0,t="p", pch=16, col="white",
             ylim=c(min(mon_prob, tue_prob, wed_prob, thu_prob, fri_prob, sat_prob, sun_prob)-0.01, max(mon_prob, tue_prob, wed_prob, thu_prob, fri_prob, sat_prob, sun_prob)+0.01),
             xlim=c(1,7),axes=F,
             xlab="",ylab="WD fraction (PRCP >= 1 mm)",
             main=paste0(station.name),cex.main=1.8,cex.lab=1.3)
        axis(1,w.day,day.names,cex.axis=1.5)
        axis(2, cex.axis=1.5,las=0.05)
        abline(h=wetall/daysall,lty=2,col="black")
        
        x <- c(1:7)
        
        par(new=T)
        plot(output, type="h", lwd=8, pch=4, col=ifelse(x==ci_out, "red", "black"), ylim=c(min(mon_prob, tue_prob, wed_prob, thu_prob, fri_prob, sat_prob, sun_prob)-0.01, max(mon_prob, tue_prob, wed_prob, thu_prob, fri_prob, sat_prob, sun_prob)+0.01), axes=F, xlab="",ylab="",cex=1)
        text(output,labels = c(mon_all, tue_all, wed_all, thu_all, fri_all, sat_all, sun_all), pos = 3)
        
        graphics.off()
      }
    }
  }
}

#################################################################################
### weekly PRCP cycles (annual)
#################################################################################

if (param.PRCP == "Y"){
  
  if(weeklycycles.complete.plots == "Y"){
    
    print("Calculating annual weekly precipitation cycles")
    
    setwd(dir.PRCP)
    dailydata.files<-list.files()
    
    ##################################################
    # find first start and last end year
    ##################################################
    start <- c(1:length(dailydata.files)) * NA
    end <- c(1:length(dailydata.files)) * NA
    for(filenumber in 1:length(dailydata.files)){
      filename<-dailydata.files[filenumber]
      station.name=(strsplit(filename, "\\.dat"))
      tab.dailydata<-read.table(filename,skip=skip.lines)
      start[filenumber] <- min(tab.dailydata[,2])
      end[filenumber] <- max(tab.dailydata[,2])
    }
    
    ##################################################
    # create dummy data.table to fill in if a year of a station is affected
    ##################################################
    
    ci_table <- as.data.frame(matrix((c(1:(length(dailydata.files)+1) * (max(end)-min(start)+2)) *NA), nrow=(max(end)-min(start)+2), ncol=(length(dailydata.files)+1)))
    ci_table[2:(max(end)-min(start)+2),1] <- c(min(start):max(end))
    ##################################################
    
    #Repeat the srcript for all files in the folder
    for(filenumber in 1:length(dailydata.files)){
      #Read the station name out of the filename
      filename <- dailydata.files[filenumber]
      station.name=paste0(strsplit(filename, "\\.dat"))
      
      #Read the file as a table
      tab.dailydata <- read.table(filename,skip=skip.lines)
      tab.dailydata[,5][tab.dailydata[,5] == -999.9] <- NA
      tab.dailydata[,5][tab.dailydata[,5] < threshold.PRCP.min] <- NA
      tab.dailydata[,5][tab.dailydata[,5] > threshold.PRCP.max] <- NA
      
      #Count days with values no NA
      if (length(which(!is.na(tab.dailydata[,5]))) < station.days) {
        print(paste0("PRCP ", station.name, ": too few observations"))
        
      } else {
        
        print(paste0("PRCP ",station.name))
        
        ci_vec <- (1 : (max(tab.dailydata[,2]) - min(tab.dailydata[,2] )+ 1)) * NA # dummie vector for yearly - ci indication
        ci_vec_pos <- 1
        
        for (year in min(tab.dailydata[,2]) : max(tab.dailydata[,2])){ #LOOP to get year by year
          get_yr <- tab.dailydata[,2] == year
          singel_yr <- tab.dailydata[get_yr,]
          
          if (is.na(mean(singel_yr[,5],na.rm=T))){ #exclude years which do only contain NAs
            ci_vec[ci_vec_pos] <- NA
          } else {
            
            # Convert the data into right format (yyyy-mm-dd to "weekday dd mm yyyy)
            datetmp <- paste(singel_yr[,4],".",singel_yr[,3],".",singel_yr[,2],sep="")
            rdate <- strftime((strptime(datetmp,format="%d. %m. %Y")),format="%a %d %m %Y")
            
            # create data.frame "dailydata" with to columns "rdate" and "ramount"
            ramount<-as.numeric(singel_yr[,5])
            dailydata<-data.frame(rdate,ramount)
            #Seperate the date into 3 single rows [yyyy,mm,dd]
            weekday<-substr(rdate,1,2)
            yr<-substr(dailydata[,1],10,14)
            m<-substr(dailydata[,1],7,8)
            d<-substr(dailydata[,1],4,5)
            
            # Years, Month, Days and "ramount" together in one data frame
            tab.data<-data.frame(yr,m,d,weekday,ramount)
            
            m<-as.numeric(m)
            
            wetdays<-ramount
            
            for (i in 1:length(ramount)){
              wetdays[i]<-ifelse(ramount[i]>=1,1,0)
            }
            
            raindays.count <- aggregate(wetdays,list(tab.data[,"weekday"]),FUN=sum,na.rm=T) # no. of wetdays per weekday
            alldays <- replace(wetdays, wetdays == 0, 1)
            weekdays.count <- aggregate(alldays,list(tab.data[,"weekday"]),FUN=sum,na.rm=T) # total no. of days per weekday
            
            mon_wd <- raindays.count[5,2]
            tue_wd <- raindays.count[1,2]
            wed_wd <- raindays.count[4,2]
            thu_wd <- raindays.count[2,2]
            fri_wd <- raindays.count[3,2]
            sat_wd <- raindays.count[6,2]
            sun_wd <- raindays.count[7,2]
            
            mon_all <- weekdays.count[5,2]
            tue_all <- weekdays.count[1,2]
            wed_all <- weekdays.count[4,2]
            thu_all <- weekdays.count[2,2]
            fri_all <- weekdays.count[3,2]
            sat_all <- weekdays.count[6,2]
            sun_all <- weekdays.count[7,2]
            
            wetall <- mon_wd+tue_wd+wed_wd+thu_wd+fri_wd+sat_wd+sun_wd
            daysall <- mon_all+tue_all+wed_all+thu_all+fri_all+sat_all+sun_all
            
            phi <- wetall/daysall # probability of a day to be a wd
            
            if (mon_all == 0){
              ciind_mon <- NA
            } else {
              ci_mon <- binom.test(mon_wd, mon_all, p = phi, alternative ="two.sided", conf.level = ci.val) # binomail test if data in agreement with null hypothesis
              if (!(ci_mon$conf.int[1] < phi & phi < ci_mon$conf.int[2])){
                ciind_mon <- 1
              } else {
                ciind_mon <- 0
              }
            }
            if (tue_all == 0){
              ciind_tue <- NA
            } else {
              ci_tue <- binom.test(tue_wd, tue_all, p = phi, alternative ="two.sided", conf.level = ci.val) # binomail test if data in agreement with null hypothesis
              if (!(ci_tue$conf.int[1] < phi & phi < ci_tue$conf.int[2])){
                ciind_tue <- 2
              } else {
                ciind_tue <- 0
              }
            }
            if (wed_all == 0){
              ciind_wed <- NA
            } else {
              ci_wed <- binom.test(wed_wd, wed_all, p = phi, alternative ="two.sided", conf.level = ci.val) # binomail test if data in agreement with null hypothesis
              if (!(ci_wed$conf.int[1] < phi & phi < ci_wed$conf.int[2])){
                ciind_wed <- 3
              } else {
                ciind_wed <- 0
              }
            }
            if (thu_all == 0){
              ciind_thu <- NA
            } else {
              ci_thu <- binom.test(thu_wd, thu_all, p = phi, alternative ="two.sided", conf.level = ci.val) # binomail test if data in agreement with null hypothesis
              if (!(ci_thu$conf.int[1] < phi & phi < ci_thu$conf.int[2])){
                ciind_thu <- 4
              } else {
                ciind_thu <- 0
              }
            }
            if (fri_all == 0){
              ciind_fri <- NA
            } else {
              ci_fri <- binom.test(fri_wd, fri_all, p = phi, alternative ="two.sided", conf.level = ci.val) # binomail test if data in agreement with null hypothesis
              if (!(ci_fri$conf.int[1] < phi & phi < ci_fri$conf.int[2])){
                ciind_fri <- 5
              } else {
                ciind_fri <- 0
              }
            }
            if (sat_all == 0){
              ciind_sat <- NA
            } else {
              ci_sat <- binom.test(sat_wd, sat_all, p = phi, alternative ="two.sided", conf.level = ci.val) # binomail test if data in agreement with null hypothesis
              if (!(ci_sat$conf.int[1] < phi & phi < ci_sat$conf.int[2])){
                ciind_sat <- 6
              } else {
                ciind_sat <- 0
              }
            }
            if (sun_all == 0){
              ciind_sun <- NA
            } else {
              ci_sun <- binom.test(sun_wd, sun_all, p = phi, alternative ="two.sided", conf.level = ci.val) # binomail test if data in agreement with null hypothesis
              if (!(ci_sun$conf.int[1] < phi & phi < ci_sun$conf.int[2])){
                ciind_sun <- 7
              } else {
                ciind_sun <- 0
              }
            }
            ci_out <- c(ciind_mon,ciind_tue,ciind_wed,ciind_thu,ciind_fri,ciind_sat,ciind_sun)
            
            mon_prob <- mon_wd/mon_all
            tue_prob <- tue_wd/tue_all
            wed_prob <- wed_wd/wed_all
            thu_prob <- thu_wd/thu_all
            fri_prob <- fri_wd/fri_all
            sat_prob <- sat_wd/sat_all
            sun_prob <- sun_wd/sun_all
            d.count <- c(mon_prob, tue_prob, wed_prob, thu_prob, fri_prob, sat_prob, sun_prob)
            w.day<-c(1:7)
            output<-data.frame(w.day,d.count)
            
            ci_vec[ci_vec_pos] <- ifelse (sum(ci_out, na.rm=T) == 0, 0, 1) #ci vector, 0 = in ci, 1 = outside ci
          }
          
          ci_vec_pos_new <- ci_vec_pos + 1 #position in ci-vec (changes with each year)
          ci_vec_pos <- ci_vec_pos_new
        }
        ci_table[1,filenumber+1] <- station.name #write station name in table
        ci_table[(min(tab.dailydata[,2])-min(start)+2) : (max(tab.dailydata[,2])-min(start)+2), filenumber+1] <- ci_vec #fill data in ci_table
      }
    }
    
    #################################################
    # print data table in ggplot
    ################################################# 
    
    # structure data to plot in ggplot (column station.name, column year, column ci_index)
    year_vec <- rep(ci_table[2:nrow(ci_table),1], ncol(ci_table)-1)
    station_vec <- year_vec * NA
    pos_first <- 1
    pos_last <- nrow(ci_table)-1
    for(name in 2:ncol(ci_table)){
      station_vec[pos_first:pos_last] <- rep(ci_table[1,name], nrow(ci_table)-1)
      pos_first <- pos_first+nrow(ci_table)-1
      pos_last <- pos_last+nrow(ci_table)-1
    }
    data_vec <- year_vec * NA
    pos_first <- 1
    pos_last <- nrow(ci_table)-1
    for(name in 2:ncol(ci_table)){
      data_vec[pos_first:pos_last] <- ci_table[2:nrow(ci_table),name]
      pos_first <- pos_first+nrow(ci_table)-1
      pos_last <- pos_last+nrow(ci_table)-1
    }
    ggplot_table <- data.frame(station_vec, year_vec, data_vec)
    name.vec <- as.vector(unique(ggplot_table[,1][!is.na(ggplot_table[,1])]))
    stations.page <- 20
    plot.no <- ceiling(length(name.vec)/stations.page)
    
    #plot in ggplot
    for(station.no in 1:plot.no){
      start.no <- station.no*stations.page-stations.page+1
      end.no <- min(station.no*stations.page, length(name.vec))
      table <- ggplot_table[which(ggplot_table[,1]  %in% name.vec[start.no:end.no]),]
      mypath <- paste0(dir.save, "/PRCP/weekly_cycles_annual_plots/weekly_cycles_annual_plots_", station.no, ".pdf")
      pdf(file=mypath,width=9,height=10.5/20*length(start.no:end.no))
      colnames(table)<-(c("station","year","ind"))
      plot <- ggplot(data=table, aes(x=year, y=station, colour=ind)) + geom_point(size=1.5) + scale_color_manual(breaks = c("0","1"),values=c("black","red")) + facet_wrap(~ station, scales = 'free_y', ncol = 1) + theme(legend.position="none", axis.title.y=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), panel.grid.major.y = element_blank(), panel.grid.major = element_line(colour = "white",size=1), axis.text=element_text(size=18), plot.margin = unit(c(0.5,1,0.5,0.5), "cm"), axis.ticks.length=unit(0.5,"cm")) + xlim(min(ggplot_table[,2]), max(ggplot_table[,2]))
      print(plot + theme(axis.text.y = element_blank(), axis.ticks = element_blank(), axis.text.y = element_blank()))
      graphics.off()
    }
  }
}