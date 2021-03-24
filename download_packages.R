# devtools 패키지 설치
install.packages("devtools")

# CARN에 등재되지 않은 패키지 설치
devtools::install_github("wilkelab/cowplot")
install.packages("colorspace", dependencies = TRUE, INSTALL_opts = '--no-lock', repos = "http://R-Forge.R-project.org")
devtools::install_github("clauswilke/colorblindr")
devtools::install_github("clauswilke/dviz.supp")
devtools::install_github("wilkelab/ungeviz")
devtools::install_github("clauswilke/ggtextures")
devtools::install_github("thomasp85/transformr")
devtools::install_github("hrbrmstr/statebins")
install.packages('rworldmap',dependencies=TRUE) 

# CRAN에 등재된 패키지 설치
install.packages(c(
  "bookdown", "cowplot",
  "tidyverse", "rgeos", "patchwork", "lwgeom",
  "ggthemes", "ggforce", "ggmap", "geofacet",
  "ggstance", "treemapify", "ggrepel",
  "tidybayes", "ggstance", "plot3D", "tinter",
  "ggridges", "emmeans", "mgcv", "sf", "showtext",
  "magick", "nycflights13", "rwordlmap",
  "gganimate"
))