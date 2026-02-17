library(shiny)
library(shinydashboard)
library(shinycssloaders)

library(rhandsontable)
library(tidyverse)
library(plotly)
library(collapsibleTree)

#library(future)
library(daewr)
library(emmeans)

library(DT)
#library(hrbrthemes)
#library(viridis)
library(ggpubr)
library(pwr)

#library(webshot)
#webshot::install_phantomjs(force=TRUE)

#library(parallelly)
#library(promises)

theme_set(
  theme_linedraw() +
    theme(
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 22),
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18, angle=90),
      strip.text = element_text(size = 18),
      plot.title = element_text(size = 18,margin = margin(b = 20))
    )
)

source("Utils.R")