# STA141A FINAL PROJECT: Analysis of Auto-mpg dataset


## Set up 

* `git clone git@github.com:gitmebrandonhom/sta141a_project.git` into whatever directory



## Shiny app 

For a more interactive experience with the data analysis check out the shiny app.

* The shiny app can be found here: http://bwhom.shinyapps.io/141a_app
  
  * The first tab displays the PCA analysis along with zooming into the divided quadrants. 
  * The second tab interfaces the predictive model. On the same page, there are tabs that contain the data. Try entering the features for the one of the data points and see how far the model is from the actual value :). 
  * The last tab is simulating a repeated train-test split. 

* The PCA plot, predictive model, and the repeated train-test split were integrated into the app. 

* After one minute of inactivity, the app will switch to idle and shut down after 5 minutes. 


## List of external library names used 

* library(leaps)
* library(GGally)
* library(ggbiplot)
* library(caret)
* library(RColorBrewer)
* library(dendextend)
* library(cowplot)
* library(kableExtra)
* library(ggpubr)
