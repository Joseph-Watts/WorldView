# modules/models/models_global.R

## packages
library("phylolm")
library("plotly")
library("pROC")

## Models sub modules
source("modules/models/phylo_lm/phylo_lm_utils.R")
source("modules/models/phylo_lm/phylo_lm_ui.R")
source("modules/models/phylo_lm/phylo_lm_server.R")

source("modules/models/phylo_glm/phylo_glm_utils.R")
source("modules/models/phylo_glm/phylo_glm_ui.R")
source("modules/models/phylo_glm/phylo_glm_server.R")

## data load
## phylo_viz_global.R already loaded