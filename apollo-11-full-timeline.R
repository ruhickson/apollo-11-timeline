# Load libraries
library(rvest)
library(dplyr)
library(ggplot2)
library(png)
library(grid)
library(magick)
library(stringr)

# Scrape Full List of Events from NASA's website
apollo11<-read_html("https://history.nasa.gov/SP-4029/Apollo_11i_Timeline.htm") %>% 
  html_nodes("body div table") %>%
  html_table(header = TRUE, trim = TRUE, fill = FALSE, dec = ".")

# Formatting and data cleansing: Keep Event and UTC Time data
apollo11 <- as.data.frame(apollo11) 
apollo11 <- apollo11[,c(1:2)]
names(apollo11)[1] <- "Milestone"
names(apollo11)[2] <- "Time"
apollo11$Milestone <- str_replace(apollo11$Milestone, " \r\n      ", " ")

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
apollo11 <- na.omit(apollo11)

# Generate timeline plot
aplot <- ggplot(apollo11,aes(reorder(Time, sort(Time)), y=0, label=Milestone))+
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

# Export image of appropriate size and scale
ggsave("apollo-11-full-timeline.png", plot=aplot, width = 150, height = 5, limitsize=FALSE)
