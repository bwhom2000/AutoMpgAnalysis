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

# model coefs from cross validation 
model.coef <- data.frame(features=c("Intercept","cylinders6","cylinders 8","displacement","horsepower","weight","acceleration","originJapan","originUSA","Horsepower^2"),
                         coefficients=c(1.955e+00,-4.665e-02,-3.399e-02,-9.599e-05,-4.513e-03,-4.304e-05,-6.536e-03,3.016e-02,5.084e-03,9.702e-06))

# TRAIN_TEST SIMULATION 
b <- n.build.and.evaluate(100,data.transformed,.8)



# Define UI for application 
ui <- navbarPage("Auto-Mpg Analysis",
  tabPanel("PCA",
  fluidPage(
  theme=shinytheme("cerulean"),
  titlePanel("Auto-mpg PCA"),
  div("Zoom into the four quadrants of the PCA plot by selecting the given options",style="color:ForestGreen"),
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
           div("The value ranges from the dataset are given and the values already in the box are just place holders.",
               style="color:ForestGreen"),
           div("It is possible to go way above the ranges, but the prediction may not be accurate!",style="color:Crimson"),
           div("Enter your car's features:",style="color:ForestGreen"),
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
              tabsetPanel(
                tabPanel("Model Prediction",span(textOutput("pred.result"), style="color:MediumVioletRed")),
                tabPanel("Model Coefficients",tableOutput("model.coefs")),
                tabPanel("Untransformed data",dataTableOutput("untransformed.data")),
                tabPanel("Log 10 Transformed MPG",dataTableOutput("transformed.data"))
              )

            )
           )
         )), 
tabPanel("Train-Test simulation",
         fluidPage(
           titlePanel("Simulate a Train-test split N times"),
           div("The purpose of this simulation is to show the randomness of a train test split
               and to see how the train and test perform based on the split size.",style="color:ForestGreen"),
           sidebarLayout(
             sidebarPanel(
               div("Graph only displays 105 repetitions!",style="color:Red"),
               numericInput("nreps","Enter number of repetitions",value=50),
               sliderInput("size","Slide to desired size. An input of 80 means 80 for training 20 for testing",min=10,max=100,value=80)
             ),
             mainPanel(
               plotOutput("RMSE.sim"),
               plotOutput("RSQ.sim")
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
    output$pca.importance.table <- renderTable(pcs.importance,digits=3)
    output$pca.corr <- renderTable(corr.matrix.df,width = "50%",digits=3)
# Prediction output 
    #get coef for num cylinders
    encode_cylind <- function(n.cylin){
      if(n.cylin=="6") return(-4.665e-02 )
      else if (n.cylin=="8") return (-3.399e-02)
      else return (0)
    }
    encode_country <- function(country){
      if (country=="USA") return (5.084e-03)
      else if (country=="Japan") return ( 3.016e-02 )
      else return (0)
    }
    
    numerical.inputs <- reactive({ sum(c(-9.599e-05,-4.513e-03,-4.304e-05,-6.536e-03,9.702e-06)*
                        c(input$displacement,input$horsepower,input$weight,input$acceleration,input$horsepower^2)) })
    
    pred.result <- reactive({numerical.inputs()+encode_cylind(input$cylinders)+encode_country(input$origin)+1.955e+00})
    
    output$pred.result <- renderText({
      paste("The predicted fuel economy in miles per gallon:",10^pred.result())
      })
    
    output$model.coefs <- renderTable(model.coef,digits=10)
    output$untransformed.data <- renderDataTable(data)
    output$transformed.data <- renderDataTable(data.transformed)
    
#TRAIN TEST SIMULATOR OUTPUT 
   
    new_dat <- reactive({n.build.and.evaluate(input$nreps,data.transformed,input$size/100)})
    output$RMSE.sim <- renderPlot(ggplot(data=new_dat(),aes(x=Trial.number))+
                                    labs(title="Train-Test n Times: RMSE", x="Trial Number", y="RMSE")+
                                    theme_light()+
                                    geom_line(aes(y=Train.RMSE,col="Train.RMSE"))+
                                    geom_line(aes(y=Test.RMSE,col="Test.RMSE"))+
                                    coord_cartesian(xlim=c(0,100),ylim=c(0.045,.08))+
                                    scale_x_continuous(breaks=seq(0,100,5))+
                                    scale_y_continuous(breaks=seq(0.045,0.08,0.005))+
                                    scale_color_manual(values = c(Train.RMSE="#E31A1C",Test.RMSE="#33A02C"), labels = c("Train", "Test"))+ 
                                    theme(legend.box.background = element_rect(linetype="solid", colour ="#984EA3", size=1.25),
                                          legend.title = element_text(face="bold", hjust = .5),
                                          legend.text = element_text(face="bold"),
                                          panel.grid.minor.x = element_blank(),
                                          axis.title = element_text(size=15),
                                          axis.text = element_text(size=10),
                                          plot.title = element_text(hjust = .5, size = 20))+
                                    guides(colour=guide_legend("RMSE"))
    )
    output$RSQ.sim <- renderPlot(ggplot(data=new_dat(),aes(x=Trial.number))+
                                   labs(title="Train-Test n Times: R Squared", x="Trial Number", y="R Squared")+
                                   theme_light()+
                                   geom_line(aes(y=Train.R.Squared,col="Train.R.Squared"))+
                                   geom_line(aes(y=Test.R.Squared,col="Test.R.Squared"))+
                                   scale_color_manual(values = c(Train.R.Squared="#E31A1C",Test.R.Squared="#33A02C"), labels = c("Train", "Test"))+ 
                                   coord_cartesian(xlim=c(0,100),ylim=c(.5,1))+
                                   scale_x_continuous(breaks=seq(0,100,5))+
                                   scale_y_continuous(breaks=seq(.5,1,0.05))+
                                   theme(legend.position="right",
                                         legend.box.background = element_rect(linetype="solid", colour ="#984EA3", size=1.25),
                                         legend.title = element_text(face="bold", hjust = .5),
                                         legend.text = element_text(face="bold"),
                                         panel.grid.minor.x = element_blank(),
                                         axis.title = element_text(size=15),
                                         axis.text = element_text(size=10),
                                         plot.title = element_text(hjust = .5, size = 20))+
                                   guides(colour=guide_legend("R Squared")))
    
}
# Run the application 
shinyApp(ui = ui, server = server)
