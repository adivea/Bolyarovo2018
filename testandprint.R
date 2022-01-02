# Bolyarovo 2018 Analysis
# follows 01_ElenovoMounds.R from Elenovo2017 folder

# libraries
library(tidyverse)
library(googlesheets4)

# load data
m18 <- read_csv("data/Bolyarovo2018clean.csv")
m2018 <- read_sheet("https://docs.google.com/spreadsheets/d/1XCgQqd7ooP4CrDYQ2EmhR3_P1VDFpYlyTxywTB3PqWI/edit#gid=1326323943") 


#check goodness of data
colnames(m2018)
summary(as.numeric(m2018$HeightMax)) # sanity check of heights range
hist(as.numeric(m2018$HeightMax), na.rm = TRUE)


# check categories of mound type (via conversion to factor) 
m2018 %>% 
  group_by(Type_Adela) %>% 
  tally()

m2018 %>% 
  filter(Type_Adela == "Burial Mound?") %>% 
  select(TRAP, createdBy,Date, Source, DescriptionOfMoundOrLocale, AllNotes, CommentsAndRecommendations)

m2018 %>% 
  select(HeightMax, DiameterMax) %>% 
  glimpse()
# ok, diameter is character

m2018$DiameterMax <- as.numeric(m2018$DiameterMax)

#### COndition

m18 %>% 
  filter(Type_Adela == "Burial Mound" |
           Type_Adela == "Extinct Burial Mound" ) %>% 
  #| Type_Adela == "Uncertain Feature") %>% 
  group_by(Condition) %>% 
  summarize(Count = n()) %>% 
  mutate(Percent = round(Count/sum(Count)*100, digits = 0))

# Check for Bunkers and Military items
grep("military", m2018$PrincipalSourceOfImpact)
m_note <- grep("milit*|bunker|tank*", m2018$PrincipalSourceOfImpactNote)
m_desc <- grep("milit*|bunker|tank*", m2018$DescriptionOfMoundOrLocale)
m_note %in% m_desc # 5 overlapping 

dim(m2018)  # 282 features
bunkers <- unique(c(m_note,m_desc)) # 46 military features

m2018[m2018$Type_Adela == "Other",]%in%m2018[bunkers,] # none

m2018 %>% 
  slice(bunkers) %>% 
  select(Type_Adela, createdBy) %>% 
  group_by(Type_Adela, createdBy) %>% 
  tally()

m2018 %>% 
  slice(bunkers) %>% 
  select(Condition, Type_Adela, createdBy) %>% 
  group_by(Type_Adela, Condition) %>% 
  tally()

m2018 %>% 
  # slice(bunkers) %>% 
  # filter(Type_Adela == "Other") %>% 
  group_by(Source) %>% 
  tally()


m2018 %>% 
  filter(Type_Adela == "Other")  # 49 features that are 'other'

# Calculate Height statistics per type of features
TypeM <- m2018 %>% 
  group_by(Type) %>% 
  tally()
TypeM

MedianH <- m2018 %>% 
  group_by(Type) %>% 
  summarize(medianHeight=median(HeightMax, na.rm=TRUE),
            medianDiam=median(DiameterMax, na.rm=TRUE)) 
MedianH

MeanH <- m2018 %>% 
  group_by(Type) %>% 
  summarize(minHeight=min(HeightMax, na.rm=TRUE), 
            maxHeight=max(HeightMax,na.rm=TRUE), 
            meanHeight = round(mean(HeightMax, na.rm=TRUE), digits = 2)) 
MeanH  # beware of the Uncertain feature if reusing

MeanDiam <- m2018 %>% 
  group_by(Type) %>% 
  summarize(minDiam=min(DiameterMax, na.rm=TRUE), 
            maxDiam=max(DiameterMax,na.rm=TRUE), 
            meanDiam = round(mean(DiameterMax, na.rm=TRUE), digits = 2)) 
MeanDiam

Feature_stats <- cbind(TypeM,MeanH[,2:4], MeanDiam[, -1])

#Feature_stats <- cbind(TypeM[-3:-4,],MeanH[-3:-4,2:4], MeanDiam[-3:-4, -1])
Feature_stats <- Feature_stats %>% 
  arrange(desc(n)) 
Feature_stats[,-c(3,6:8)]

# Write the results out
write_csv(m2018, "../Elenovo2017/data/Bolyarovo2018clean.csv")
write_csv(Feature_stats, "../Elenovo2017/output/Bolyarovo_Dimensions_2018.csv")
dim(m2018)
grep("bunker", m2018)
# Calculate statistics by source (survey or legacy data verification)

Source <- m2018 %>% 
  group_by(Source) %>% 
  tally()
Source

SourceStats <- m2018 %>% 
  group_by(Source) %>% 
  summarize(minHeight=min(HeightMax, na.rm=TRUE), 
            maxHeight=max(HeightMax,na.rm=TRUE), 
            meanHeight = mean(HeightMax, na.rm=TRUE), 
            minDiam=min(DiameterMax, na.rm=TRUE), 
            maxDiam=max(DiameterMax,na.rm=TRUE), 
            meanDiam = mean(DiameterMax, na.rm=TRUE)) 
SourceStats

Source_stats <- cbind(Source,SourceStats[,-1])  # eliminate duplicate column
write_csv(Source_stats, 'Ele_Sourcestats.csv')


### Create a BoxPlot comparing the height distributions of m2018 (exluding other stuff)


# index burial and uncertain m2018
m2018$Type[m2018$Type=="Burial Mound?"] <- "Uncertain Feature"

TypeM
m2018_index <- which(m2018$Type_Adela=="Burial Mound")
uncertain_index <- which(m2018$Type_Adela=="Uncertain Feature"| m2018$Type_Adela=="Burial Mound?")
extinct_index <- which(m2018$Type_Adela=="Extinct Burial Mound")

# create boxplot of heights for mound phenomena (no surf. scatter or other)
mound_index <- m2018[c(m2018_index,extinct_index,uncertain_index),]

head(mound_index)
boxplot(HeightMax~Type, mound_index, las = 1)

boxplot(DiameterMax~Type, mound_index, las = 1)

?pdf
pdf("../Elenovo2017/output/2018Combined.pdf", 13, 5 )
# run the code below to generate the figure for pdf:

par(mfrow=c(1,2)) # to combine the two plots below horizontally

boxplot(HeightMax~Type, data = mound_index, 
        # col = gray.colors(3),
        main = "Height distribution",
        xlab = "", # to eliminate Type as x label
        ylab = "meters", cex.lab = 1.3,    #cex = increases symbols in plot, cex.lab - increases axis labels
        cex.axis = 1,                      #cex.axis = increases data labels
        las = 1) # rotate y axis

boxplot(DiameterMax~Type, mound_index,
        # col = gray.colors(3),
        main = "Diameter distribution",
        ylab = "",
        xlab = "",   
        cex.axis = 1,                      #cex.axis = increases data labels
        las = 1) # rotate y axis

dev.off()

pdf("output/Figure03bw.pdf", width = 7, height = 3.5)
par(mfrow=c(1,2))  # set plotting into a 1*2 array
boxplot(HeightMax~Type_Adela, data = mounds_index,
        main = "Height distribution",
        xlab = "",  # skip name on x axis
        ylab = "meters",   # relabel y axis
        cex.lab = 1.3, #cex = increases symbols in plot, cex.lab - increases axis labels
        cex.axis = 1,                      #cex.axis = increases data labels
        las = 1) # rotate y axis 
boxplot(DiameterMax~Type_Adela, mounds_index,  
        main = "Diameter distribution",
        xlab = "",
        ylab = "meters", cex.lab = 1.3,
        cex.axis = 1,
        las = 1) 
dev.off()



#########################BOXPLOT VERTICAL 

require(gridExtra)
hplot <- ggplot(mounds_index, aes(Type_Adela, HeightMax))+
  geom_boxplot(alpha = 0.4)+
  labs(x = NULL, y = "Height (m)") +
  scale_x_discrete(limits = rev(levels(as.factor(mounds_index$Type_Adela))))+
  theme_bw()+
  coord_flip()
#theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
dplot <- ggplot(mounds_index, aes(Type_Adela, DiameterMax))+
  # geom_violin(color = "salmon")+
  # geom_jitter(color = "salmon")+
  geom_boxplot(alpha = 0.4)+
  labs(x = NULL, y = "Diameter (m)") +
  scale_x_discrete(limits = rev(levels(as.factor(mounds_index$Type_Adela))))+
  theme_bw()+
  coord_flip()
theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

grid.arrange(hplot, dplot, ncol=1)
##################################################
###  CONDITION BARPLOT FOR PRINTING

# Figure 07
pdf("output/Figure07bw.pdf", width = 7, height = 3.5)
m18 %>% 
  filter(Type_Adela == "Burial Mound" |
           Type_Adela == "Extinct Burial Mound" ) %>% 
  #| Type_Adela == "Uncertain Feature") %>% 
  ggplot(aes(x = Condition, fill = Type_Adela)) +
  geom_bar(width = 0.75) +
  labs(x = NULL, y = NULL, fill = "Feature Type") +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(as.factor(m18$Condition))))+
  theme_bw() +
  scale_fill_grey()+
  theme(axis.text = element_text(size = 12),
        legend.position=c(.75,.85))
dev.off()

# FIgure 08
pdf("output/Figure08bw.pdf", width = 7, height = 4)
m18 %>% 
  filter(Type_Adela == "Burial Mound" |
           Type_Adela == "Extinct Burial Mound" |
           Type_Adela == "Uncertain Feature" | 
           Type_Adela == "Other" )  %>% # 207 features
  ggplot(aes(x = Condition, fill = Type_Adela)) +
  geom_bar(width = 0.75) +
  labs(x = NULL, y = NULL, fill = "Type:") +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(as.factor(m18$Condition))))+
  theme_bw() +
  scale_fill_grey()+
  theme(axis.text = element_text(size = 12),
        legend.position="bottom")
dev.off()
####################################################
### Wish to try a Shiny application? 
### Run the 02_interactive_data_explorer.R


### Streamlining Mound Condition (can be done in OpenRefine)
levels(factor(m2018$Condition))

m2018 <- m2018 %>%
  mutate(Condition = str_extract(Condition, "\\d")) %>%
  mutate(Condition = case_when(Condition == 0 ~ "NA",
                               #  Condition == 6 ~ "5",
                               Condition != 0 ~ Condition))
m2018$Condition <- as.numeric(m2018$Condition)
unique(m2018$Condition)

write_csv(m2018, "output/Condition.csv")

####################################################
### Wish to create a map?
## Playing with mound height visualisation 

p <- ggplot(mound_index, aes(Type, HeightMax, color=Type)) +
  geom_violin(trim=FALSE)
p
# violin plot with mean points
p + stat_summary(fun=mean, geom="point", shape=23, size=2)
# violin plot with median points
p + stat_summary(fun=median, geom="point", size=2, color="red")
# violin plot with jittered points
# 0.2 : degree of jitter in x direction
p + geom_jitter(shape=16, position=position_jitter(0.2))


# Get a tally of visited features by team leader and day
m2018 %>% 
  group_by(createdBy) %>% 
  tally()

# Review the progress of individual teams
teamprogress <- m2018 %>% 
  group_by(createdBy, Date) %>% 
  tally()

teamprogress %>% 
  arrange(desc(n))


## Create a quick Map
library(leaflet)

map <- leaflet() %>% 
  addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
  addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
  addCircleMarkers(lng = as.numeric(m2018$Longitude),
                   lat = as.numeric(m2018$Latitude),
                   #radius = m2018$HeightMax, group="Legacy",
                   radius = m2018$Condition, group="Legacy",
                   popup = paste0("MoundID: ", m2018$identifier,
                                  "<br> Height: ", m2018$HeightMax,
                                  "<br> Robber's trenches: ", m2018$RTDescription)) %>% 
  addLayersControl(
    baseGroups = c("Topo","ESRI Aerial"),
    overlayGroups = c("Legacy"),
    options = layersControlOptions(collapsed = T))

map


#### Condition
unique(m2018$Condition)
TypeM

m <- m2018 %>%
  mutate(Condition = str_extract(Condition, "\\d")) %>%
  mutate(Condition = case_when(Condition == 0 ~ "NA",
                             #  Condition == 6 ~ "5",
                               Condition != 0 ~ Condition),
         Condition = factor(Condition, levels = c(1,2,3,4,5, NA))) 
m2018$Condition

map <- leaflet() %>% 
  addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
  addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
  addCircleMarkers(data = st_transform(Bol_mounds$geometry, crs = 4326),
                   radius = m2018$HeightMax, group="Legacy",
                   # radius = m$Condition, group="Legacy",
                   popup = paste0("MoundID: ", m$identifier,
                                  "<br> Height: ", m$HeightMax,
                                  "<br> Robber's trenches: ", m$RTDescription)) %>% 
  addLayersControl(
    baseGroups = c("Topo","ESRI Aerial"),
    overlayGroups = c("Legacy"),
    options = layersControlOptions(collapsed = T))

map


##### Improved Leaflet map  - switch on internet!
library(leaflet)
l_bol <- leaflet() %>%   # assign the base location to an object
  setView(26.80, 42.15, zoom = 10)
  


esri <- grep("^Esri", providers, value = TRUE)

for (provider in esri) {
  l_bol <- l_bol %>% addProviderTiles(provider, group = provider)
}

Blgmap <- l_bol %>%
  addLayersControl(baseGroups = names(esri),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addMiniMap(tiles = esri[[1]], toggleDisplay = TRUE,
             position = "bottomright") %>%
  addMeasure(
    position = "bottomleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479") %>% 
  htmlwidgets::onRender("
                        function(el, x) {
                        var myMap = this;
                        myMap.on('baselayerchange',
                        function (e) {
                        myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
                        })
                        }") %>% 
  addControl("", position = "topright")
Blgmap %>% 
  addCircleMarkers(data = st_transform(Bol_mounds$geometry, crs = 4326))


### TMAPS

tm_shape(Balkans$geometry, bbox = BalkanBox)+
  tm_polygons(col = "white", border.col = "darkgrey")+
  tm_shape(Bg)+
  tm_polygons(col = "lightgrey", 
              border.col = "darkgrey",
              lwd = 1 )+
  tm_shape(Yam$geometry) +
  tm_polygons(col = "lightgrey", border.col = "darkgrey") +
  tm_shape(Bol_mun$geometry) +
  tm_polygons(col = "black", border.col = "black")+
  tm_shape(YambolBox) +
  tm_polygons(alpha = 0, border.col = "red")

# Draw bounding box  / extent manually
plot(Balkans$geometry)
e <- drawExtent(show = TRUE, col = "red")
