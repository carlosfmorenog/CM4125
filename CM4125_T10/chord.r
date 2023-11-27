# Source: https://www.data-to-viz.com/graph/chord.html

#### Set WD ####
setwd("C:/Users/CM8738/Dropbox/RGU/Teaching/2023-2024/Sem 1/CM4125 - Data Vis/CM4125 T10")

#### Load libraries ####
library(readxl)
library(tidyverse)
library(networkD3)
library(reshape2)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(circlize)

##### Load data #####
data <- read_excel("chord.xlsx")

##### HEATMAP #####

## Create an empty canvas
heatmap_colnames=colnames(data)
mat_heatmap = matrix(0, length(data), length(data))
rownames(mat_heatmap) <- colnames(data)
colnames(mat_heatmap) <- colnames(data)

## For loop to count all combinations
# Notice this is different to sankey, as it has to count all rows
for (i in 1:length(data)){
  for (j in 1:length(data)){
    if (i>j){
      for (k in 1:nrow(data)){
        row = data[k,]
        if (is.na(row[i])==FALSE & is.na(row[j])==FALSE){
          if (row[i]=="Yes" & row[j]=="Yes"){
            mat_heatmap[heatmap_colnames[i],heatmap_colnames[j]]=mat_heatmap[heatmap_colnames[i],heatmap_colnames[j]]+1
          }
        }
      }
    }
  }
}


## Simple heatmap (rough idea)
heatmap(mat_heatmap, Colv = NA, Rowv = NA, scale="column")

##### PLOTTING A CHORD DIAGRAM ######

mat_heatmap = data.frame(mat_heatmap)
## Transform matrix to long data
data_long_chord <- mat_heatmap %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname)

## Basic parameters
circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))

## Establish colour palette
mycolor <- viridis(length(unique(data_long_chord$rowname)), 
                   alpha = 1, begin = 0, end = 1, option = "D")
mycolor <- mycolor[sample(1:length(unique(data_long_chord$rowname)))]

## Base plot
chordDiagram(
  x = data_long_chord, 
  grid.col = mycolor,
  transparency = 0.25,
  directional = 1,
  direction.type = c("arrows", "diffHeight"), 
  diffHeight  = -0.04,
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow", 
  link.sort = TRUE, 
  link.largest.ontop = TRUE)

## Add text and axis
circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    
    # Add names to the sector. 
    circos.text(
      x = mean(xlim), 
      y = 3.2, 
      labels = sector.index, 
      facing = "bending", 
      cex = 0.8
    )
  }
)

##### Another option #####
## Source: https://r-graph-gallery.com/chord-diagram-interactive.html?utm_content=cmp-true

## Install packages
library(devtools)
# devtools::install_github("mattflor/chorddiag")
library(chorddiag)

## We need a full matrix, so let's do it again!
mat_chord = matrix(0, length(data), length(data))
chordmap_colnames=colnames(data)
rownames(mat_chord) <- colnames(data)
colnames(mat_chord) <- colnames(data)
for (i in 1:length(data)){
  for (j in 1:length(data)){
      for (k in 1:nrow(data)){
        if (i!=j){
        row = data[k,]
        if (is.na(row[i])==FALSE & is.na(row[j])==FALSE){
          if (row[i]=="Yes" & row[j]=="Yes"){
            mat_chord[chordmap_colnames[i],chordmap_colnames[j]]=mat_chord[chordmap_colnames[i],chordmap_colnames[j]]+1
          }
        }
      }
    }
  }
}

## Plot the chord with one line of code!
chord <- chorddiag(as.matrix(mat_chord), groupColors = mycolor, groupnamePadding = 20)
chord # You can save it as an image or as html from the viewer tab
