# Home module ----
home_mod <- function(id){
  ns <- NS(id)
  homepage_ui()
  }

# LCA module ----
lca_mod <- function(id, project){
  ns <- NS(id)
  lca_ui(project)
}

# LPA module ----
lpa_mod <- function(id, project){
  ns <- NS(id)
  lpa_ui(project)
}
# LPT module ----
lta_mod <- function(id, project){
  ns <- NS(id)
  lta_ui(project)
}
# EFA module ----
efa_mod <- function(id, project){
  ns <- NS(id)
  efa_ui(project)
}
# CFA module ----
cfa_mod <- function(id, project){
  ns <- NS(id)
  cfa_ui(project)
}
