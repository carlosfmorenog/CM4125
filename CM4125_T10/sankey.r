# Source: https://rpubs.com/YJ_Choi/FPDynamicsData
# Adding title to Sankeys: https://stackoverflow.com/questions/50132459/how-to-add-title-to-a-networkd3-visualisation-when-saving-as-a-web-page

#### Set WD ####
setwd("C:/Users/CM8738/Dropbox/RGU/Teaching/2023-2024/Sem 1/CM4125 - Data Vis/CM4125 T10")

#### Load libraries ####
library(readxl)
library(tidyverse)
library(networkD3)
library(reshape2)
library(ggplot2)

#### Load data ####
data <- read_excel("sankey.xlsx")

##### Sankey with two columns #####

## Get unique values from the two columns
a=sort(unique(data$Gender))
b=sort(unique(data$Household_Income))

## Create a zeros matrix
mat = matrix(0, length(a), length(b))
rownames(mat) <- sort(a)
colnames(mat) <- sort(b)

## Count the number of times one value is related to the other
for (x in 1:nrow(data)){
    row = data[x,]
    m = row$Gender
    n = row$Household_Income
    if (is.na(m)==FALSE & is.na(n)==FALSE){
      mat[m,n]=mat[m,n]+1
    }
}

## Convert mat into a data frame (easier to handle)
mat=as.data.frame(mat)

## Reshape data to long format 
data_long <- mat %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname) %>%
  filter(value > 0)
colnames(data_long) <- c("source", "target", "value")

# From these flows we need to create a node data frame: 
# it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(data_long$source),
                           as.character(data_long$target)) %>% unique())

# With networkD3, connection must be provided using id, 
# This allows us to know who gets connected
# Notice the -1, our IDs will go from 0 to n (JavaScript)
data_long$IDsource=match(data_long$source, nodes$name)-1
data_long$IDtarget=match(data_long$target, nodes$name)-1

# Make the Network
# set "iterations=0" to avoid automatic assignment of the box order
sankey<- sankeyNetwork(Links = data_long, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE, nodeWidth=40, fontSize=13, 
              nodePadding=20, iterations=0)

## Show the sankey plot!
sankey # You can save it as an image or as html from the viewer tab


##### Sankey with three columns #####

## Add a third column
c=sort(unique(data$`Would you stop eating meat?`))

## Create a new zeros matrix
mat2 = matrix(0, length(b), length(c))
rownames(mat2) <- b
colnames(mat2) <- c

## Another count
for (x in 1:nrow(data)){
    row = data[x,]
    m = row$Household_Income
    n = row$`Would you stop eating meat?`
    if (is.na(m)==FALSE & is.na(n)==FALSE){
      mat2[m,n]=mat2[m,n]+1
    }
  } 

## As data frame
mat2=as.data.frame(mat2)

# Reshape data to long format 
data_long2 <- mat2 %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname) %>%
  filter(value > 0)
colnames(data_long2) <- c("source", "target", "value")


## Create node data
nodes2 <- data.frame(name=c(as.character(data_long2$source),
                           as.character(data_long2$target)) %>%unique())


## Source & target
data_long2$IDsource=match(data_long2$source, nodes2$name)-1 
data_long2$IDtarget=match(data_long2$target, nodes2$name)-1


## Make the Three Column Diagram
newnodes_col3 <- data.frame(name=c(as.character(data_long2$target)) %>%
                               unique())
nodes_3cols <-rbind(nodes, newnodes_col3)
data_long_3cols <-rbind(data_long, data_long2)
# Remove the previous matching
data_long_3cols <- subset(data_long_3cols, select = c("source", "target", "value"))
# New matching
data_long_3cols$IDsource=match(data_long_3cols$source, nodes_3cols$name)-1
data_long_3cols$IDtarget=match(data_long_3cols$target, nodes_3cols$name)-1

## Make the new network
sankey2<- sankeyNetwork(Links = data_long_3cols, Nodes = nodes_3cols,
                        Source = "IDsource", Target = "IDtarget",
                        Value = "value", NodeID = "name", 
                        sinksRight=FALSE, nodeWidth=40, fontSize=13, 
                        nodePadding=20, iterations=0)
sankey2 # Note that at this point we cannot know gender/diet change relation. For thatm we would need to invert the order!
