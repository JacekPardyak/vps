if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))


# -------------------------------------------------------------------------------------------
library(tidyverse)
library(h2o)
h2o.init()
prostate.hex = h2o.uploadFile(path = "./shiny-h2o/prostate.csv",
                              destination_frame = "prostate")

prostate <- h2o.importFile(path = "./shiny-h2o/prostate.csv")                                    

summary(prostate)

prostate_glm <- h2o.glm(y = "CAPSULE", x = c("AGE", "RACE", "PSA", "DCAPS"),
                        training_frame = prostate, family = "binomial", alpha = 0.5)
h2o.saveModel(object = prostate_glm, 
              path = "./shiny-h2o/", force = TRUE)
prostate_glm_ <- h2o.loadModel(path = "./shiny-h2o/GLM_model_R_1645556006926_1")

tbl <- tibble(ID = 100,
              AGE = 10,
              RACE = 1,
              DPROS = 2,
              DCAPS = 1,
              PSA = 4,
              VOL = 0,
              GLEASON = 6) %>% 
  as.h2o()

h2o.predict(object = prostate_glm_, newdata = tbl)

h2o.shutdown(prompt = F)                        
