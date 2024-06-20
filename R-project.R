#################################installing packages#######################################
install.packages("readxl")
install.packages("reader")
install.packages("dplyr")
install.packages("tidyverse")
install.packages(c("shiny", "tidyverse"))
install.packages("arules")
install.packages("formattable")
install.packages("arulesViz")
############################################################################################ 
#cleaning......#
############################################################################################
library("readxl")
library(reader)
Data <- read.csv(readline("Enter the Data path: "))
sum(duplicated(Data))
library(dplyr)
Data_Cleaning1=distinct(Data)
sum(duplicated(Data_Cleaning1))
sum(is.na(Data_Cleaning1))
boxplot(Data_Cleaning1[2:4])
outlier1 = boxplot(Data_Cleaning1$count)$out
Data_Cleaning1[which(Data_Cleaning1$count%in% outlier1),]
Data_Cleaning2 = Data_Cleaning1[-which(Data_Cleaning1$count%in% outlier1),]
boxplot(Data_Cleaning2$count)$out
boxplot(Data_Cleaning2$rnd)$out
boxplot(Data_Cleaning2[2:4])
Data_Cleaning2

####################################### #Visualizations ###############################################
library(ggplot2)



###1

table(Data_Cleaning2$paymentType)
x<-table(Data_Cleaning2$paymentType)
percentage =paste0( round( (x/sum(x)) * 100 , 2 ),"%")
percentage
pie(x ,main="Cash vs Credit",labels=percentage,col=c("pink","lightblue"))
legend("bottomright", legend = c("Cash", "Credit"), fill = c("pink","lightblue"))

###2

library(tidyverse)

total_spending_by_age <- Data_Cleaning2 %>%
  group_by(age) %>%
  summarise(totalA = sum(total))%>%
  arrange(desc(totalA))

barplot(
  height = total_spending_by_age$totalA,
  names.arg = total_spending_by_age$age,
  col = "skyblue",
  main = "Total spending vs Ages",
  xlab = "Age",
  ylab = "Total Spending"
)

###3

total_spending_by_city <- Data_Cleaning2 %>%
  group_by(city) %>%
  summarise(totalC = sum(total)) %>%
  arrange(desc(totalC))

plot(reorder(total_spending_by_city$city, -total_spending_by_city$totalC), 
     total_spending_by_city$totalC, 
     xlab = "City", 
     ylab = "Spending", 
     main = "Total spending for each city")

###4


boxplot(Data_Cleaning2$total, main=" the distribution of total spending. ", xlab="Total spending")


#########################################Clustring #################################################
library(tidyverse)

data(Data_Cleaning2)
plot(Data_Cleaning2) # show data items

Data_to_clustering <- Data_Cleaning2 %>%
  group_by(customer , age) %>%
  summarise(Totalcl = sum(total))

Data_Scaled <-c(scale(Data_to_clustering$Totalcl) , scale(Data_to_clustering$age)) #convert data to scale before clustering
Data_Scaled
Data_clustring<- kmeans( Data_Scaled, centers = 
as.integer(readline("Enter Number of clusters between 2 and 4: ")) ) # clustering
Data_clustring
plot(Data_Cleaning2 , col = Data_clustring$cluster) # visualize clusters

Data_with_clusters<- data.frame(name = Data_to_clustering$customer , 
                                age= Data_to_clustering$age ,
                                total = Data_to_clustering$Totalcl,
                                cluster = Data_clustring$cluster )
Data_with_clusters
###################################### association rule##########################################
library("arules")
Data_association <- read.transactions(textConnection(Data_Cleaning2$items) , sep=",")

Data_apriori<- apriori( Data_association , parameter = list(
supp= as.numeric(readline("Enter the support value Number between 0.001 and 1: ")) ,
conf= as.numeric(readline("Enter the Confidence value Number between 0.001 and 1: ")),  minlen=2 ) )
Data_apriori
print(Data_apriori)
inspect(Data_apriori )




##########################################GUI########################################################
library(shiny)
library("readxl")
library(reader)
library("arules")
library(tidyverse)
library(dplyr)
library(ggplot2)




ui <- fluidPage(
  titlePanel("Data science project - Team 79"),
  
  tabsetPanel(
    tabPanel("Data Cleaning",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Choose CSV File"),
                 verbatimTextOutput("cleaned_summary")
               ),
               mainPanel(
                 plotOutput("boxplots")
               )
             )
    ),
    
    tabPanel("Visualization",
             sidebarLayout(
               sidebarPanel(
                 selectInput("plotType", "Select Plot Type",
                             choices = c("Comparing cash and credit totals",
                                         "Comparing each age and sum of total spending",
                                         "Show Total city spending",
                                         "The distribution of total spending",
                                         "Dashboard"))
               ),
               mainPanel(
                 plotOutput("plot")
               )
             )
    ),
    
    tabPanel("Data Clustering Visualization",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("clusters", "Number of clusters:", min = 2, max = 4, value = 2),
                 br(),
                 actionButton("clusterButton", "Cluster Data")
               ),
               
               mainPanel(
                 plotOutput("plot_cluster"),
                 dataTableOutput("table_cluster")
               )
             )
    ),
    
    tabPanel("Association Rule Mining",
             sidebarLayout(
               sidebarPanel(
                 textInput("support", "Enter the support value (between 0.001 and 1):", ""),
                 textInput("confidence", "Enter the confidence value (between 0.001 and 1):", ""),
                 actionButton("runButton", "Run Apriori")
               ),
               mainPanel(
                 verbatimTextOutput("aprioriOutput")
               )
             )
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$runButton, {
    support <- as.numeric(input$support)
    confidence <- as.numeric(input$confidence)
    
    if (is.na(support) || support < 0.001 || support > 1 || is.na(confidence) || confidence < 0.001 || confidence > 1) {
      output$aprioriOutput <- renderPrint({
        "Invalid input. Please enter support and confidence values between 0.001 and 1."
      })
    } else {
      Data_association <- read.transactions(textConnection(Data_Cleaning2$items) , sep=",")
      Data_apriori<- apriori( Data_association , parameter = list( supp= support , conf= confidence , minlen=2 ) )
      
      if(length(Data_apriori) == 0){
        output$aprioriOutput <- renderPrint({
         "NO association rules found. "
        })
      }else if(length(Data_apriori)!= 0 ){
      output$aprioriOutput <- renderPrint({
        inspect(Data_apriori)
      })
      }
    }
  })
  
  cleaned_data <- reactive({
    req(input$file)
    Data <- read.csv(input$file$datapath)
    Data_Cleaning1 <- distinct(Data)
    outlier_indices <- boxplot(Data_Cleaning1$count)$out
    Data_Cleaning2 <- Data_Cleaning1[!Data_Cleaning1$count %in% outlier_indices, ]
    Data_Cleaning2
  })
  
  output$cleaned_summary <- renderPrint({
    summary(cleaned_data())
  })
  
  output$boxplots <- renderPlot({
    boxplot(cleaned_data()[2:4])
  })
  
  output$plot <- renderPlot({
    plot_type <- input$plotType
    
    if (plot_type == "Comparing cash and credit totals") {
      x <- table(cleaned_data()$paymentType)
      percentage <- paste0(round((x / sum(x)) * 100, 2), "%")
      pie(x, main = "Cash vs Credit", labels = percentage, col = c("pink", "lightblue"))
      legend("bottomright", legend = c("Cash", "Credit"), fill = c("pink", "lightblue"))
      
    } else if (plot_type == "Comparing each age and sum of total spending") {
      total_spending_by_age <- cleaned_data() %>%
        group_by(age) %>%
        summarise(totalA = sum(total)) %>%
        arrange(desc(totalA))
      
      barplot(
        height = total_spending_by_age$totalA,
        names.arg = total_spending_by_age$age,
        col = "skyblue",
        main = "Total spending vs Ages",
        xlab = "Age",
        ylab = "Total Spending"
      )
      
    } else if (plot_type == "Show Total city spending") {
      total_spending_by_city <- cleaned_data() %>%
        group_by(city) %>%
        summarise(totalC = sum(total)) %>%
        arrange(desc(totalC))
      
      plot(reorder(total_spending_by_city$city, -total_spending_by_city$totalC), 
           total_spending_by_city$totalC, 
           xlab = "City", 
           ylab = "Spending", 
           main = "Total spending for each city")
      
    } else if (plot_type == "The distribution of total spending") {
      boxplot(cleaned_data()$total, main = "The distribution of total spending", xlab = "Total spending")
      
    } else if (plot_type == "Dashboard") {
      par(mfrow=c(2,2))
      install.packages("tidyverse")
      library(ggplot2)
      
      x <- table(cleaned_data()$paymentType)
      percentage <- paste0(round((x / sum(x)) * 100, 2), "%")
      pie(x ,main="Cash vs Credit",labels=percentage,col=c("pink","lightblue"))
      legend("bottomright", legend = c("Cash", "Credit"), fill = c("pink","lightblue"))
      
      total_spending_by_age <- cleaned_data() %>%
        group_by(age) %>%
        summarise(totalA = sum(total)) %>%
        arrange(desc(totalA))
      
      barplot(
        height = total_spending_by_age$totalA,
        names.arg = total_spending_by_age$age,
        col = "skyblue",
        main = "Total spending vs Ages",
        xlab = "Age",
        ylab = "Total Spending"
      )
      
      total_spending_by_city <- cleaned_data() %>%
        group_by(city) %>%
        summarise(totalC = sum(total)) %>%
        arrange(desc(totalC))
      
      plot(reorder(total_spending_by_city$city, -total_spending_by_city$totalC), 
           total_spending_by_city$totalC, 
           xlab = "City", 
           ylab = "Spending", 
           main = "Total spending for each city")
      
      boxplot(cleaned_data()$total, main="The distribution of total spending", xlab="Total spending")
    }
  })
  
  clusteredData <- reactive({
    req(input$clusterButton)
    
    Data_to_clustering <- Data_Cleaning2 %>%
      group_by(customer , age) %>%
      summarise(Totalcl = sum(total))
    
    Data_Scaled <- Data_Scaled <-c(scale(Data_to_clustering$Totalcl) , scale(Data_to_clustering$age)) #convert data to scale before clustering

    k <- kmeans(Data_Scaled, centers = input$clusters)
    Data_with_clusters <- data.frame(name = Data_to_clustering$customer,
                                     age = Data_to_clustering$age,
                                     total = Data_to_clustering$Totalcl,
                                     cluster = k$cluster)
    return(Data_with_clusters)
  })
  
  output$plot_cluster <- renderPlot({
    plot(clusteredData()$total, clusteredData()$age, col = clusteredData()$cluster, pch = 19, main = "Data Clustering")
    points(clusteredData()$total, clusteredData()$age, col = clusteredData()$cluster, pch = 19)
    legend("topright", legend = paste("Cluster", unique(clusteredData()$cluster)), col = unique(clusteredData()$cluster), pch = 19)
  })
  
  output$table_cluster <- renderDataTable({
    clusteredData()
  })
}

shinyApp(ui = ui, server = server)