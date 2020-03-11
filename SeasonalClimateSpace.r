library(raster)
library(ecoclim)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(rgdal)
library(weathermetrics)
library(sf)
library(here)
library(arcgisbinding)

arc.check_product()

outdir <- here::here("output") #I:/projects/NCR/Workspace/auer/Visualization/climate_change_exposure_charts/climate_space_charts/climate_space_sample_topowx/"

#get clip feature
sites <- arc.open("E:/ClimateChange/Peatlands/PeatlandClimateChange/PeatlandClimateChange.gdb/HIEL_AllSites_2019_10kmBuffer")
sites <- arc.select(sites)
sites <- arc.data2sf(sites)

# note: build in checks for multipart polygons and makes sure they are unique

# ##### Load climate normals for bio1 and bio12 #####
# baseline <- parseMetadata("I:/climate_data/TopoWx/v2014/derived/normals/biovars/rasters_means/")
# future <- parseMetadata("I:/climate_data/ClimateNA/future/derived/RCP85_ensemble_2025/biovars")
# d <- rbind(baseline, future)

# #Colors
# old_colors <- c("1948-1980" = "deepskyblue3","1981-2014" = "chartreuse3", "2011-2040"= "firebrick3")
# pinks <- c("1948-1980" = "#fde0dd","1981-2014" = "#fa9fb5", "2011-2040"= "#c51b8a")
# purples <- c("1948-1980" = "#e0ecf4","1981-2014" = "#9ebcda", "2011-2040"= "#8856a7")
# #greenblue <- c("1948-1980" = "#67a9cf","1981-2014" = "#1c9099", "2011-2040"= "#016c59") # original
# greenblue <- c("1948-1980" = "#a8ddb5","1981-2014" = "#43a2ca", "2011-2040"= "#0868ac") # more contrast
greens <- c("baseline" = "#a1d99b", "current" = "#31a354", "near-future"= "#006d2c")
# greenblue <- c("1948-1980" = "#67a9cf","1981-2014" = "#02818a", "2011-2040"= "#014636")
# set1 <- c("baseline" = "#377eb8","current" = "#4daf4a", "near-future"= "#984ea3")
# set2 <- c("1948-1980" = "#4daf4a","1981-2014" = "#984ea3", "2011-2040"= "#ff7f00")
# set3 <- c("1948-1980" = "#4daf4a","1981-2014" = "#377eb8", "2011-2040"= "#e41a1c")
# greens <- set1

# load in the summer temp and precip rasters   
btemp <- raster(here::here("baseline","bio10.asc"))#d$path[d$variable=="bio10" & d$year==1980])
rtemp <- raster(here::here("observed","bio10.asc"))#d$path[d$variable=="bio10" & d$year==2014])
ftemp <- raster(here::here("nearfuture","bio10.asc"))#d$path[d$variable=="bio10" & d$year==2025])
bppt <- raster(here::here("baseline","bio16.asc"))#d$path[d$variable=="bio16" & d$year==1980])
rppt <- raster(here::here("observed","bio16.asc"))#d$path[d$variable=="bio16" & d$year==2014])
fppt <- raster(here::here("nearfuture","bio16.asc"))#d$path[d$variable=="bio16" & d$year==2025])
# load in the winter temp and precip rasters 
Wbtemp <- raster(here::here("baseline","bio11.asc")) #(d$path[d$variable=="bio11" & d$year==1980])
Wrtemp <- raster(here::here("observed","bio11.asc")) #(d$path[d$variable=="bio11" & d$year==2014])
Wftemp <- raster(here::here("nearfuture","bio11.asc")) #(d$path[d$variable=="bio11" & d$year==2025])
Wbppt <- raster(here::here("baseline","bio17.asc")) #(d$path[d$variable=="bio17" & d$year==1980])
Wrppt <- raster(here::here("observed","bio17.asc")) #(d$path[d$variable=="bio17" & d$year==2014])
Wfppt <- raster(here::here("nearfuture","bio17.asc")) #(d$path[d$variable=="bio17" & d$year==2025])


# loop through for analysis and graphs
for(p in 1:nrow(sites)){
  site <- sites[p,]

  ## create random sample of points within park boundary
  library(stars)
  x = read_stars(here::here("nearfuture","bio16.asc"))
  st_crs(x) <- st_crs(sites)
  x1 <- st_crop(x, site)
  points <- st_as_sf(x1, as_points=TRUE)
  

  # extract to points
  btemp1 <- raster::extract(btemp, points, method='simple', df=TRUE)
  names(btemp1) <- c("ID", "temp")
  btemp1$year <- "baseline"
  rtemp1 <- raster::extract(rtemp, points, method='simple', df=TRUE)
  names(rtemp1) <- c("ID", "temp")
  rtemp1$year <- "current"
  bppt1 <- raster::extract(bppt, points, method='simple', df=TRUE)
  names(bppt1) <- c("ID", "precip")
  bppt1$year <- "baseline"
  rppt1 <- raster::extract(rppt, points, method='simple', df=TRUE)
  names(rppt1) <- c("ID", "precip")
  rppt1$year <- "current"
  ftemp1 <- raster::extract(ftemp, points, method='simple', df=TRUE)
  names(ftemp1) <- c("ID", "temp")
  ftemp1$year <- "near-future"
  fppt1 <- raster::extract(fppt, points, method='simple', df=TRUE)
  names(fppt1) <- c("ID", "precip")
  fppt1$year <- "near-future"
  
  dft <- rbind(btemp1, rtemp1, ftemp1)
  dfp <- rbind(bppt1, rppt1, fppt1)
  rm(btemp1, rtemp1, ftemp1, bppt1, rppt1, fppt1)
  summer_tempprecip <- merge(dft, dfp)
  summer_tempprecip <- summer_tempprecip[complete.cases(summer_tempprecip),]

  ########### same for winter variables ##################
  Wbtemp1 <- raster::extract(Wbtemp, points, method='simple', df=TRUE)
  names(Wbtemp1) <- c("ID", "temp")
  Wbtemp1$year <- "baseline"
  Wrtemp1 <- raster::extract(Wrtemp, points, method='simple', df=TRUE)
  names(Wrtemp1) <- c("ID", "temp")
  Wrtemp1$year <- "current"
  Wbppt1 <- raster::extract(Wbppt, points, method='simple', df=TRUE)
  names(Wbppt1) <- c("ID", "precip")
  Wbppt1$year <- "baseline"
  Wrppt1 <- raster::extract(Wrppt, points, method='simple', df=TRUE)
  names(Wrppt1) <- c("ID", "precip")
  Wrppt1$year <- "current"
  Wftemp1 <- raster::extract(Wftemp, points, method='simple', df=TRUE)
  names(Wftemp1) <- c("ID", "temp")
  Wftemp1$year <- "near-future"
  Wfppt1 <- raster::extract(Wfppt, points, method='simple', df=TRUE)
  names(Wfppt1) <- c("ID", "precip")
  Wfppt1$year <- "near-future"
  
  Wdft <- rbind(Wbtemp1, Wrtemp1, Wftemp1)
  Wdfp <- rbind(Wbppt1, Wrppt1, Wfppt1)
  winter_tempprecip <- merge(Wdft, Wdfp)
  winter_tempprecip <- winter_tempprecip[complete.cases(winter_tempprecip),]
  
  # mean centers for arrows
  summer_means <- aggregate(summer_tempprecip[, 3:4], list(summer_tempprecip$year), mean)
  names(summer_means)[which(names(summer_means)=="Group.1")] <- "year"
  winter_means <- aggregate(winter_tempprecip[, 3:4], list(winter_tempprecip$year), mean)
  names(winter_means)[which(names(winter_means)=="Group.1")] <- "year"
  
  
  ############ Version with legend  ###############
  library(gridExtra)
  p1 <- ggplot() +
    geom_point(data=summer_tempprecip, aes(x=temp, y=precip, color=year),  size=1) + 
    geom_path(data=summer_means, aes(x=summer_means$temp, y=summer_means$precip), lwd=2, color="black", alpha=0.9, arrow=arrow(length=unit(4.2, "mm"))) +
    geom_path(data=summer_means, aes(x=summer_means$temp, y=summer_means$precip), lwd=1.1, color="grey82", alpha=0.9, arrow=arrow(length=unit(4, "mm"))) +
    scale_x_continuous(breaks=seq(floor(min(summer_tempprecip$temp,na.rm = T)), ceiling(max(summer_tempprecip$temp,na.rm = T)), 1)) +
    scale_color_brewer(type="qual", palette = "Dark2") +
    ggtitle("Summer") +
    theme(plot.title = element_text(size=6, margin=margin(4,0,0,0))) +
    labs(x="Temp of Warmest Quarter (°C)", y="Precip of Wettest Quarter (mm)") +
    theme(aspect.ratio=1) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.spacing.x = unit(0.02, 'cm'),
      panel.grid.minor = element_blank(),
      axis.text = element_text(colour="gray42", size=9),
      text = element_text(size=8)
    ) 
  
  p2 <- ggplot() +
    geom_point(data=winter_tempprecip, aes(x=temp, y=precip, color=year), size=1) + 
    geom_path(data=winter_means, aes(x=winter_means$temp, y=winter_means$precip), lwd=2, color="black", alpha=0.9, arrow=arrow(length=unit(4.2, "mm"))) +
    geom_path(data=winter_means, aes(x=winter_means$temp, y=winter_means$precip), lwd=1.1, color="grey82", alpha=0.9, arrow=arrow(length=unit(4, "mm"))) +
    scale_x_continuous(breaks=seq(floor(min(winter_tempprecip$temp,na.rm = T)), ceiling(max(winter_tempprecip$temp,na.rm = T)), 1)) +
    scale_color_brewer(type="qual", palette = "Dark2") +
    ggtitle("Winter") +
    theme(plot.title = element_text(size=6, margin=margin(4,0,0,0))) +
    labs(x="Temp of Coldest Quarter (°C)", y="Precip of Driest Quarter (mm)") +
    theme(aspect.ratio=1) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      axis.text = element_text(colour = "gray42", size = 9),
      text= element_text(size=8)
          ) 
 
  #extract legend -- https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  mylegend<-g_legend(p1)  
   
  big_plot <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"), p2 + theme(legend.position="none"), top=textGrob(site$SITE), nrow=1, ncol=2), mylegend, nrow=2,heights=c(10, 1)) 
  
  ggsave(paste0(outdir,"/", site$SITE, "_SeasonalChange.png"),  big_plot, width=2.5, height=1.25, units="in", scale=2)  
  dev.off()

}  
