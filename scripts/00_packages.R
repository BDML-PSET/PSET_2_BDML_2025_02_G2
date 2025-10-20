library(pacman)
p_load(tidyverse,rio,janitor,data.table,fastDummies, ### Data wrangling
       tidymodels,caret, ### Metapackages
       glmnet,ranger,xgboost,baguette,rpart,ipred,bonsai,gbm, ### Machine learning algorithms
       pROC,themis,finetune,future,parallel,doParallel,MLmetrics,tictoc, ### Other packages required
       jsonlite, httr,kableExtra)
options(tidymodels.dark = T)

