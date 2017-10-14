#'Visualize airport delays
#'
#'@field 
#'
#'@details Plot of geographical position of airports of the "flights" dataset with their mean delay.
#'@examples visualize_airport_delays()
#'@export visualize_airport_delays

#require nycflights13 package and ggplot2, dplyr and ggrepel <- already in description

visualize_airport_delays<-function(){
  
  data<-data.frame(name=airports$faa,longitude=airports$lon,latitude=airports$lat)
  airport<-unique(c(unique(flights$origin),unique(flights$dest))) #airports flights informations
  
  ariport_dest<-unique(flights$dest)
  positions<-data[data[,1] %in% airport,]
  
  #destination, arrival delay mean
  delay_mean_dest <-data.frame(name=flights$dest,del=flights$arr_delay) %>% 
               group_by(name) %>% 
               summarize(mean=round(mean(del, na.rm=TRUE)))
  delay_mean_dest <- delay_mean_dest[which(!is.na(delay_mean_dest[,2])),]
  
  delay_mean_dep <-data.frame(name=flights$origin,del=flights$dep_delay) %>% 
                  group_by(name) %>% 
                  summarize(mean=round(mean(del, na.rm=TRUE)))
  
  #union between departure airport and arrival airport
  un<-union(mutate(delay_mean_dest,name=factor(name,levels=airport)),mutate(delay_mean_dep,name=factor(name,levels=airport)))
  
  #conclusive dataframe with all we need to plot the mean delay
  info<-left_join(mutate(positions,name=factor(name,levels=airport)),mutate(un,name=factor(name,levels=airport)),by="name")
 
  
  fig<-ggplot(info, aes(x=longitude,y=latitude,col=mean)) + geom_text_repel(aes(label=name), size = 3,col="black") + geom_point(size=7,shape=20,stroke=1) + labs(x = "Longitude", y = "Latitude") +
    ggtitle("Mean delay of flights")+scale_colour_gradient(low = "white", high = "red")+geom_text(aes(x=longitude,y=latitude, label=mean),col="black",size=3,parse=TRUE)+scale_y_continuous()+scale_x_continuous()+
    theme(panel.border = element_blank(),
          plot.title = element_text(size = rel(1.5), face='bold',vjust = 1.5,hjust = 0.5),
          axis.ticks = element_blank(),
          plot.margin = unit(c(1,5,1,4), "cm")
          )
  return(fig)
}