library(ggplot2)
library(sf)
library(patchwork)#for overlaying plot


#--------------------------------------------------------------------------
# Load shape files of state, Chesapeake Bay Watershed, and US map outlines
#--------------------------------------------------------------------------
#get shape file for Chesapeake Bay Watershed from here:
#https://hub.arcgis.com/datasets/ChesBay::chesapeake-bay-watershed-boundary/about
cbw_shape <- read_sf(dsn = "directory where Chesapeake Bay Watershed outline shape file is saved",
                 layer = "Chesapeake_Bay_Watershed_Boundary")

#outlines for states within the Chesapeake Bay Watershed
usa <- map_data('usa')
state <- map_data("state")
state_cbw <- state[which(state$region %in% c('new york',"virginia","pennsylvania","west virginia","maryland",'delaware',"district of columbia","new jersey","north carolina",'ohio')),]

#convert state outlines to the correct map projection
state_cbw2 <- st_as_sf(state_cbw,coords=c('long','lat'),crs=4326)

#set plot bounding box
ymin <- 36.7
ymax <- 42.87
xmin <- -81
xmax <- -74.4


#---------------------------------
#plot of Chesapeake Bay Watershed
#---------------------------------
p1<-ggplot() +
  geom_polygon(data=state_cbw, aes(x=long, y=lat, group=group),fill='white',color='gray50',size=.3)+ coord_map("rectangular") +
  theme(panel.background = element_rect(fill = 'gray30'),
        panel.grid.major = element_line(color = 'gray30'),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=.5))+
  geom_sf(data = cbw_shape,
          color = 'black', fill='gray', ## here you can set parameters just like any other ggplot
          alpha=.6,lwd=.4)+## can vary the size of the stream segment width if necessary
  coord_sf(ylim=c(ymin,ymax),xlim=c(xmin,xmax))+
  labs(y="Latitude",x="Longitude")


#---------------------------
# USA map overlay plot
#---------------------------
p2<-ggplot() +
  geom_polygon(data=usa, aes(x=long, y=lat, group=group),fill='gray30',color='gray30',linewidth=.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_rect(aes(xmin=xmin,xmax=xmax, 
                ymin=ymin, ymax=ymax),
            fill=NA,color='black',linewidth=0.4)+
  geom_rect(aes(xmin=-126.5,xmax=-65.5, 
                ymin=24.5,ymax=50),
            fill=NA,color='black',linewidth=.5)+
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'white'),
        panel.grid.minor = element_line(color = 'white'),
        axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank(),
        axis.ticks.length = unit(0, "pt"),
        plot.margin=unit(c(0,0,0,0), "mm"))
  
#--------------
#combine plots
#--------------
p1 + inset_element(p2, left = 0.01, bottom = 0.81, right = 0.38, top = .99, clip=TRUE)
