# Load libraries
library(dplyr)
library(ggplot2)
library(png)
library(grid)
library(magick)

# Create base data frame of events and times
apollo11<-data.frame(
  "Time" = c("0:00:00","2:44:16","3:24:03","75:49:50","100:39:53","102:45:40","109:07:33","124:22:01","128:03:00","195:07:15","195:18:35"),
  "Milestone" = c("Apollo 11 Launches","Loop Around Earth","Vessels Rearrange in Space","Apollo 11 Enters the Moon's Orbit","Armstrong Maneuvers Descent","The Eagle Has Landed!","One Small Step for Man...","Lunar Module Launches","Docking Procedure with Command Module","Re-entry into Earth's Atmosphere","Splashdown"))

# Create iterable postitions for data points
position <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5)
direction <- c(1, -1)
text_position <- c(0.55, -0.55, 1.05, -1.05, 1.55, -1.55)

# Create data frame for position data and iterate over Apollo data
timeline_manager <- data.frame(
  "Time"=unique(apollo11$Time),
  "position"=rep(position, length.out=length(unique(apollo11$Time))),
  "direction"=rep(direction, length.out=length(unique(apollo11$Time))),
  "text_position"=rep(text_position, length.out=length(unique(apollo11$Time)))
)

# Merge data sets
apollo11 <- inner_join(apollo11, timeline_manager, by = "Time")
apollo11$day_Time <- round(as.numeric(sub("\\:.*", "", apollo11$Time))/24,0)+1

# Generate timeline plot
aplot<-ggplot(apollo11,aes(reorder(Time, sort(as.numeric(Time))), y=0, label=Milestone))+
  labs(col = "Milestones",
       title = "Apollo 11 Timeline",
       x = "Time elapsed (HH:MM:SS)",
       caption = "created by Ru Hickson 2019-07-16")+
  theme_classic()+
  geom_hline(yintercept=0, color = "black", size=0.3)+
  geom_segment(apollo11, mapping=aes(y=position,yend=0,xend=Time), color="black", size=0.2) +
  geom_point(apollo11, mapping=aes(y=0, color=factor(day_Time), size=3))+
  theme(axis.line.y=element_blank(),axis.text.y=element_blank(),
        axis.title.y=element_blank(),axis.title.x=element_text(size=8),axis.ticks.y=element_blank(),
        axis.text.x =element_blank(),axis.ticks.x =element_blank(),
        axis.line.x =element_blank(),legend.position = "none"
  )+
  geom_text(aes(x=Time,y=-0.15,label=Time),size=2.5,vjust=-1,hjust=0.5, color='black', angle=90)+
  geom_text(aes(y=text_position,label=Milestone, color=factor(day_Time)),size=3)

# Download Apollo 11 mission logo and overlay onto plot
logo <- image_read('https://upload.wikimedia.org/wikipedia/commons/thumb/2/27/Apollo_11_insignia.png/1014px-Apollo_11_insignia.png')
aplot
grid.raster(logo, x = 0.1, y = 0.15, width = 0.1, height = 0.2)
