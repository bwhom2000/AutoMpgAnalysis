#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
library(shiny)
library(tidyverse)
library(knitr)
library(leaps)
library(GGally)
library(ggbiplot)
library(caret)
library(RColorBrewer)
library(dendextend)
library(shinythemes)
# Code from project to be interfaced with app 

# data cleaning up
data <- read.csv('../../Project_code/auto-mpg.csv')
#convert horsepower chr->dbl 
data$horsepower <- as.numeric(data$horsepower)
#remove rows with missing values
data <- na.omit(data)
#translate origin numbers to country strings
data$origin <- ifelse(data$origin==1,"USA",ifelse(data$origin==2,"Europe","Japan"))
data$origin <- as.factor(data$origin)
#cylinders count for 3 and 5 low combine with 4 and 6 respectively
data$cylinders <- replace(data$cylinders,data$cylinders %in% c(3,5),c(4,6))
data$cylinders <- as.factor(data$cylinders)
#remove model.year, not interested in this feature 
data <- data[-c(7)]
data$car.name <- word(data$car.name,1)
#car.name fix typos
data$car.name[160] <- "chevrolet"
data$car.name[330] <- "volkswagen"
data$car.name[82] <- "toyota"

#PCA
corr.matrix <- cor(data[-c(2,7,8)])
corr.matrix.df <- data.frame(rownames(corr.matrix),corr.matrix)
colnames(corr.matrix.df)[1] <- "Correlation Matrix"
pcs.out <- prcomp(data[-c(2,7,8)],scale.=T)
pcs.dat <- data.frame(rownames(pcs.out$rotation),pcs.out$rotation)
colnames(pcs.dat)[1] <- "Features"
pcs.importance <- data.frame(summary(pcs.out)[6])
pcs.importance <- cbind(c("Standard deviation","Proportion of Variance","Cumulative Proportion"),pcs.importance)
colnames(pcs.importance) <- c("Metrics","PC1","PC2","PC3","PC4","PC5")
cols <- brewer.pal(3, "Dark2")

#PCa plots
whole.pca <- ggbiplot(pcs.out,labels = data$car.name,groups=data$origin,obs.scale = 1,labels.size = 3.3)+
  geom_hline(yintercept = 0,col="hotpink3",size=.75)+
  geom_vline(xintercept = 0,col="hotpink3",size=.75)+
  theme_light()+
  scale_color_manual(values=cols)+
  theme(legend.box.background = element_rect(linetype="solid", colour ="hotpink3", size=1.25),
        legend.title = element_text(face="bold", hjust = .5),
        legend.text = element_text(face="bold"))+
  guides(colour=guide_legend("Country"))

tl.pca <- ggbiplot(pcs.out,labels = data$car.name,groups=data$origin,obs.scale = 1,labels.size = 4.6)+
  geom_hline(yintercept = 0,col="hotpink3")+
  geom_vline(xintercept = 0,col="hotpink3")+
  ylim(0,1.8)+
  xlim(-5,0)+
  theme_light()+
  theme(plot.title=element_text(hjust=.5,size=20),
        axis.text = element_text(size=15)
  )+
  labs(title="Top left",
  )+
  scale_color_manual(values=cols)+ theme(legend.box.background = element_rect(linetype="solid", colour ="hotpink3", size=1.25),
                                         legend.title = element_text(face="bold", hjust = .5),
                                         legend.text = element_text(face="bold"))+
  guides(colour=guide_legend("Country"))

tr.pca <- ggbiplot(pcs.out,labels = data$car.name,groups=data$origin,obs.scale = 1,labels.size = 4.6)+
  geom_hline(yintercept = 0,col="hotpink3")+
  geom_vline(xintercept = 0,col="hotpink3")+
  ylim(0,1.8)+
  xlim(0,2.8)+
  theme_light()+
  theme(plot.title=element_text(hjust=.5,size=20),
        axis.text = element_text(size=15)
  )+
  labs(title="Top Right",
  )+
  scale_color_manual(values=cols)+ theme(legend.box.background = element_rect(linetype="solid", colour ="hotpink3", size=1.25),
                                         legend.title = element_text(face="bold", hjust = .5),
                                         legend.text = element_text(face="bold"))+
  guides(colour=guide_legend("Country"))

bl.pca <- ggbiplot(pcs.out,labels = data$car.name,groups=data$origin,obs.scale = 1,labels.size = 4.6)+
  geom_hline(yintercept = 0,col="hotpink3")+
  geom_vline(xintercept = 0,col="hotpink3")+
  ylim(-2.5,0)+
  xlim(-4.5,0)+
  theme_light()+
  theme(plot.title=element_text(hjust=.5,size=20),
        axis.text = element_text(size=15)
  )+
  labs(title="Bottom Left",
  )+
  scale_color_manual(values=cols)+ 
  theme(legend.box.background = element_rect(linetype="solid", colour ="hotpink3", size=1.25),
        legend.title = element_text(face="bold", hjust = .5),
        legend.text = element_text(face="bold"))+
  guides(colour=guide_legend("Country"))

br.pca <- ggbiplot(pcs.out,labels = data$car.name,groups=data$origin,obs.scale = 1,labels.size = 4.6)+
  geom_hline(yintercept = 0,col="hotpink3")+
  geom_vline(xintercept = 0,col="hotpink3")+
  ylim(-3,0)+
  xlim(0,2.8)+
  theme_light()+
  theme(plot.title=element_text(hjust=.5,size=20),
        axis.text = element_text(size=15)
  )+
  labs(title="Bottom Right",
  )+
  scale_color_manual(values=cols)+
  theme(legend.box.background = element_rect(linetype="solid", colour ="hotpink3", size=1.25),
        legend.title = element_text(face="bold", hjust = .5),
        legend.text = element_text(face="bold"))+
  guides(colour=guide_legend("Country"))

# TRANSFORMED DATA 
data.transformed <- data
data.transformed$mpg <- log(data.transformed$mpg,base=10)
data.transformed <- data.transformed[-c(8)]

# PREDICTIVE MODEL FUNCTIONS 
train.test <- function(data,split.size){
  #randomize the data
  randomized.rows <- sample(nrow(data))
  randomized.data <- data[randomized.rows,]
  #split based on desired size
  split <- round(nrow(randomized.data)*split.size)
  train <- randomized.data[1:split,]
  test <- randomized.data[(split+1):nrow(randomized.data),]
  return(list(train,test))
}

#computes the Rsquared and MSE
model.metrics <- function(predicted,actual,data){
  SSE <- sum((predicted-actual)^2)
  SSTO <- sum((actual-mean(actual))^2)
  R.squared <- 1-(SSE/SSTO)
  R.MSE <- sqrt(SSE/nrow(data))
  results <- c(R.MSE,R.squared)
  names(results) <- c("RMSE","R.squared")
  return(results)
}

#From the full model:mpg~.+I(horsepower^2), specify what features to remove
build.model.features <- function(data,feats="None"){
  if(sum(!feats%in%"None")!=0) {
    #input validation
    if(sum(!feats %in% colnames(data))!=0){
      return("Error: No Such feature(s)")
    }
    features <- as.formula(paste("mpg~.+I(horsepower^2)-",paste(feats,collapse= "-")))
    return(features)
    
  }
  else return(as.formula(paste("mpg~.+I(horsepower^2)")))
}

# Combines usage of build.model.features and model.metrics to simulate a train-test split evaluation
build.and.evaluate <- function(data,split.size,feats="None"){
  #train-test split
  train <- train.test(data,split.size)[[1]]
  test <- train.test(data,split.size)[[2]]
  #build model
  model <- lm(build.model.features(data,feats),train)
  print(build.model.features(data,feats))
  #predict on test set
  p.train <- predict(model,train)
  p.test <- predict(model,test)
  #evaluate model
  metric.results <- c(model.metrics(p.train,train$mpg,train),
                      model.metrics(p.test,test$mpg,test))
  names(metric.results) <- c("Train.RMSE","Train.R.Squared","Test.RMSE","Test.R.Squared")
  return(metric.results)
}

# Runs build and evaluate n times and returns a dataframe of the results 
n.build.and.evaluate <- function(n,data,split.size,feats="None"){
  df <- data.frame(matrix(ncol=4,nrow = 0))
  for(i in 1:n){
    metric.results <- build.and.evaluate(data,split.size,feats)
    df <- rbind(df,metric.results)
  }
  df <- cbind(1:n,df)
  colnames(df) <- c("Trial.number","Train.RMSE","Train.R.Squared","Test.RMSE","Test.R.Squared")
  return(df)
}


# Define UI for application 
ui <- navbarPage("Auto-Mpg Analysis",
  tabPanel("PCA",
  fluidPage(
  theme=shinytheme("cerulean"),
  titlePanel("Auto-mpg PCA"),
  div("Zoom into the four quadrants of the PCA plot by selecting the given options",style="color:chocolate"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("pca.input",
                   "Choose to display the whole plot or zoom into a quadrant:",
                   c("Whole plot",
                     "Top Right",
                     "Top Left",
                     "Bottom Right",
                     "Bottom Left"
                     )
                  ),
      tableOutput("pca.table"),
      tableOutput("pca.importance.table")
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("PCA Plot",plotOutput("pca.plot",height=755)),
        tabPanel("Correlation Matrix",tableOutput("pca.corr"))
      )
    )
  )
  
)),
tabPanel("Predict MPG",
         fluidPage(
           titlePanel("Fuel economy predictor"),
           div("Enter your car's features:",style="color:chocolate"),
           div("The value ranges from the dataset are given and the values already in the box are just place holders"),
           sidebarLayout(
             sidebarPanel(
               radioButtons("cylinders",
                            "Select number of cylinders:",
                            c("4","6","8")
                 ),
               numericInput("displacement","Displacement (100s~300s)",value=80),
               numericInput("horsepower","Horsepower(50s~300s)",value=69),
               numericInput("weight","Weight(1000s~5000s)",value=2020,),
               numericInput("acceleration","Acceleration(10s~20s)",value=19,),
               radioButtons("origin",
                            "Select car's origin:",
                            c("USA","Japan","Europe"))
               
             ),
             
            mainPanel(
              span(textOutput("pred.result"), style="color:MediumVioletRed")
            )
           )
         ))

)
# Define server logic 
server <- function(input, output) {
#PCA OUTPUTS
    output$pca.plot <- renderPlot(
      if (input$pca.input == "Top Right")  {tr.pca}   
      else if (input$pca.input == "Top Left")  {tl.pca}  
      else if (input$pca.input == "Bottom Right")  {br.pca}  
      else if (input$pca.input== "Bottom Left")   {bl.pca}
      else {whole.pca}
    )
    output$pca.table <- renderTable(pcs.dat)
    output$pca.importance.table <- renderTable(pcs.importance)
    output$pca.corr <- renderTable(corr.matrix.df,width = "50%")
# Prediction output 
    encode_disp <- function(x){
      if(x=="4"){
        return(70)
      }
    }
    ho <- sum(c(-9.599e-05,-9.599e-05))
    pred.result <- reactive({(input$displacement*ho)+(1+2)+encode_disp(input$cylinders)})
    output$pred.result <- renderText({
      paste("The predicted fuel economy in miles per gallon:",pred.result())
      })
}
# Run the application 
shinyApp(ui = ui, server = server)
