#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
getwd()
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
pcs.out <- prcomp(data[-c(2,7,8)],scale.=T)
pcs.dat <- data.frame(rownames(pcs.out$rotation),pcs.out$rotation)
colnames(pcs.dat)[1] <- "Features"
pcs.importance <- data.frame(summary(pcs.out)[6])
pcs.importance <- cbind(c("Standard deviation","Proportion of Variance","Cumulative Proportion"),pcs.importance)
colnames(pcs.importance) <- c("Metrics","PC1","PC2","PC3","PC4","PC5")
cols <- brewer.pal(3, "Dark2")

#PCa plots
whole.pca <- ggbiplot(pcs.out,labels = data$car.name,groups=data$origin,obs.scale = 1,labels.size = 2.3)+
  geom_hline(yintercept = 0,col="hotpink3",size=.75)+
  geom_vline(xintercept = 0,col="hotpink3",size=.75)+
  theme_light()+
  scale_color_manual(values=cols)+
  theme(legend.box.background = element_rect(linetype="solid", colour ="hotpink3", size=1.25),
        legend.title = element_text(face="bold", hjust = .5),
        legend.text = element_text(face="bold"))+
  guides(colour=guide_legend("Country"))

tl.pca <- ggbiplot(pcs.out,labels = data$car.name,groups=data$origin,obs.scale = 1,labels.size = 2.3)+
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

tr.pca <- ggbiplot(pcs.out,labels = data$car.name,groups=data$origin,obs.scale = 1,labels.size = 2.3)+
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

bl.pca <- ggbiplot(pcs.out,labels = data$car.name,groups=data$origin,obs.scale = 1,labels.size = 2.3)+
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

br.pca <- ggbiplot(pcs.out,labels = data$car.name,groups=data$origin,obs.scale = 1,labels.size = 2.3)+
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




# Define UI for application 
cylinder_choices <- c(4,6,8)
ui <- fluidPage(
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
      plotOutput("pca.plot",height=760)
    )
  )
  
  

)

# Define server logic 
server <- function(input, output) {

    output$pca.plot <- renderPlot({
      if (input$pca.input == "Top Right")  {tr.pca}   
      else if (input$pca.input == "Top Left")  {tl.pca}  
      else if (input$pca.input == "Bottom Right")  {br.pca}  
      else if (input$pca.input== "Bottom Left")   {bl.pca}
      else {whole.pca}
    })
    output$pca.table <- renderTable(pcs.dat)
    output$pca.importance.table <- renderTable(pcs.importance)
}

# Run the application 
shinyApp(ui = ui, server = server)
