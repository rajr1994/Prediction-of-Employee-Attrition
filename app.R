#Load packages
library(shiny)
library(shinythemes)
library(caret)
library(xgboost)
library(dplyr)
library(sqldf)

#Read in the dataset
data <- read.csv(file="https://raw.githubusercontent.com/rajr1994/TerminatoR/master/WA_Fn-UseC_-HR-Employee-Attrition.csv",header = T)

#Shiny UI component
ui <- navbarPage(theme = shinytheme("superhero"),
                 tabPanel("Employee Attrition"),
                 tabPanel("Our Team", 
                          fluidPage(column(2,
                                           img(src="Shikhar_Jamuar.png", height='300', width='200'),
                                           em("Shikhar Jamuar", align="center")
                          ),
                          column(2,
                                 img(src="Anish_Pahwa.png", height='300', width='200'),
                                 em("Anish Pahwa", align="center")
                          ),
                          column(2,
                                 img(src="Rahul_Raj.png", height='300', width='200'),
                                 em("Rahul Raj", align="center")
                          ),
                          column(2,
                                 img(src="Vinitha_Ravindran.png", height='300', width='200'),
                                 em("Vinitha Ravindran", align="center")
                          ),
                          column(2,
                                 img(src="Mengying_Sun.png", height='300', width='200'),
                                 em("Mengying Sun", align="center")
                          ))),
                 tabPanel("Introduction and Business Problem"
                          ,fluidPage(br(),br(),br(),
                                     HTML('<center><img src="Introduction1.png" height=600 width=1100></center>')
                          )),
                 tabPanel("Model Explanatory Data Analysis",
                          fluidPage(
                            plotOutput("donut"),
                            plotOutput("column"),
                            plotOutput("line"))),
                 tabPanel("Choose Your Chart", 
                          fluidPage(
                            selectInput("variable", label = "Select a metric", choices = names(data)),
                            plotOutput("dynamic")
                          )),
                 tabPanel("Model", 
                          fluidPage(
                            titlePanel("File Input"),
                            sidebarLayout(
                              sidebarPanel(
                                fileInput("file","Upload the file"), # fileinput() function is used to get the file upload contorl option
                                helpText("Default max. file size is 5MB"),
                                tags$hr(),
                                h5(helpText("Settings to Display Data Table: ")),
                                checkboxInput(inputId = 'header', label = 'Show Header', value = FALSE),
                                checkboxInput(inputId = "stringAsFactors", "Convert String to Factors", FALSE),
                                br(),
                                radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
                              ),
                              mainPanel(
                                uiOutput("tb"),
                                dataTableOutput("new")))))
)

#Shiny server component
server <- function(input, output) {
  output$donut <- renderPlot({
    
    count.data <- data.frame(
      Attrition = as.factor(c("No","Yes")),
      n = c(sum(data$Attrition=="No"),sum(data$Attrition=="Yes")),
      prop = round(c((sum(data$Attrition=="No")*100/(sum(data$Attrition=="No") + sum(data$Attrition=="Yes"))),
                     (sum(data$Attrition=="Yes")*100/(sum(data$Attrition=="No") + sum(data$Attrition=="Yes")))),1))
    
    count.data <- count.data %>% arrange(desc(Attrition)) %>% mutate(lab.ypos = cumsum(prop) - 0.5*prop)
    
    ggplot(count.data, aes(x = 2, y = prop, fill = Attrition)) +
      geom_bar(stat = "identity", color = "white") +
      coord_polar(theta = "y", start = 0)+
      geom_text(aes(y = lab.ypos, label = paste0(prop, "%")), color = "white", size =5)+
      scale_fill_manual(values = c("aquamarine4","brown1")) +
      theme_void()+
      xlim(0.5, 2.5)+
      annotate(geom = 'text', x = 0.5, y = 0, label = nrow(data), size =14)+
      labs(title = "Overall Attrition Percent")+
      #ggtitle("Overall Attrition Percent")+
      theme(plot.title = element_text(size=20,hjust = 0.5))
  })
  
  output$column <- renderPlot({
    Income <- sqldf("select Department, avg(MonthlyIncome) as avg_Income from data group by Department")
    
    data_1 <- merge(data, Income, by="Department", all.x = T)
    
    data_1$Salary_per_Position <- ifelse(data_1$MonthlyIncome > data_1$avg_Income, "Higher than average",
                                         ifelse(data_1$MonthlyIncome < data_1$avg_Income, "Lower than average", "Equal"))
    table(data_1$Salary_per_Position, useNA = "ifany")
    
    
    data_2 <- sqldf("select data_1.Department, data_1.Salary_per_Position, (a.attr*100/count(Attrition)) as Attrition_pct
      from data_1 left join (select Department, Salary_per_Position, 
      count(Attrition) as attr from data_1 where Attrition = 'Yes' group by Department, Salary_per_Position) a
      on data_1.Department = a.Department
      and data_1.Salary_per_Position = a.Salary_per_Position
      group by data_1.Department, data_1.Salary_per_Position")
    
    ggplot(data_2, aes(factor(reorder(Department,Attrition_pct)), Attrition_pct, fill = Salary_per_Position)) + 
      geom_bar(stat="identity", position = "dodge") +
      geom_text(aes(label = paste0(Attrition_pct,"%")), size =5,
                position = position_dodge2(width = 0.8, preserve = "single"), vjust=-0.5, hjust=0)+
      labs(y= "Attrition %", x="Department", title="Relative Attrition percent")+
      scale_fill_manual(values = c("aquamarine4","brown1"))+theme(panel.background = element_rect(fill = 'white', colour = 'white'),plot.title = element_text(size=20,hjust = 0.5))
  })
  
  
  output$line <- renderPlot({
    df_2 <- sqldf("select data.JobRole, (a.attr*100/count(Age)) as Attrition_pct
      from data left join (select JobRole, count(Attrition) as attr from data where Attrition = 'Yes' group by JobRole) a
      on data.JobRole = a.JobRole
      group by data.JobRole")
    
    
    df_3 <- df_2 %>% mutate(Avg = mean(Attrition_pct, na.rm = TRUE),
                            Above_avg = ifelse((Attrition_pct - Avg) > 0, "Above Average", "Below Average")) %>% arrange(desc(Attrition_pct))
    
    
    ggplot(df_3, aes(Attrition_pct, JobRole, color = Above_avg,label = paste0(round(Attrition_pct, 0), "%"))) +
      geom_segment(aes(x = mean(df_2$Attrition_pct), y = reorder(JobRole, Attrition_pct), xend = Attrition_pct, yend = JobRole)) +
      geom_point(size = 12) +
      geom_text(color = "white", size = 4)+
      scale_fill_manual(values = c("aquamarine4","brown1")) +
      labs(x = "Attrition %", y = "Job Role", color = "Attrition %")
  })
  
  reac <- reactive({input$variable})
  options(scipen = 999)
  output$dynamic <- renderPlot({
    ggplot(data, aes_string(x=input$variable, y = nrow(data), fill = "Attrition",xlab(input$Variable))) + 
      geom_bar(stat = "identity") +
      labs(y="Percentage")+
      scale_fill_manual(values = c("aquamarine4","brown1"))+
      guides(fill = FALSE) +
      #geom_text(size = 3, hjust = 0.5, vjust = 3, position = "stack")+
      ggtitle(paste("Attrition by",input$variable))+
      theme_classic()
  })
  
  
  data_1 <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()}
    read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
  })
  
  
  
  # this reactive output contains the summary of the dataset and display the summary in table format
  
  output$filedf <- renderTable({
    if(is.null(data_1())){return ()}
    
    input$file
    
  })
  
  
  
  # this reactive output contains the summary of the dataset and display the summary in table format
  
  output$sum <- renderTable({
    
    if(is.null(data_1())){return ()}
    
    summary(data_1())
    
    
    
  })
  
  
  
  # This reactive output contains the dataset and display the dataset in table format
  
  output$table <- renderTable({
    
    if(is.null(data_1())){return ()}
    
    data_1()
    
  })
  
  
  output$tb <- renderUI({
    
    if(is.null(data_1()))
      
      h5("Powered by", tags$img(src='TerminatoR.png', heigth=200, width=200))
    
    else
      
      tabsetPanel(tabPanel("Prediction", tableOutput("filedf")),tabPanel("Data", tableOutput("table")),tabPanel("Summary", tableOutput("sum")))
    
  })
  
  output$new <- renderDataTable({
    new_data <- data[,2]
    data <- data[,-2]
    data <- cbind(new_data, data)
    names(data)[1] <- "Attrition"
    data <- subset(data, select = -Over18)
    rm(new_data)
    names(data)
    
    d <- subset(data,,c("Attrition", "EducationField", "JobRole", "MonthlyIncome", "YearsAtCompany", "Age",
                        "Department", "JobSatisfaction", "YearsInCurrentRole", "Gender"))
    
    set.seed(99)
    inTrain <- createDataPartition(y = d$Attrition,   # outcome variable
                                   p = .80,   # % of training data you want
                                   list = F)
    
    df_train <- d[inTrain,]  # training data set
    df_test <- d[-inTrain,]  # test data set
    
    # Modeling
    logit <- glm(Attrition ~ EducationField + JobRole + MonthlyIncome + YearsAtCompany + Age + Department + JobSatisfaction +
                   YearsInCurrentRole + Gender, data= df_train, family=binomial)
    
    pLogit <- predict(logit, type = "response", newdata = data_1())
    #yLogit <- ifelse(pLogit>=0.30,"Yes","No")
    results = data.frame(data_1()$Employee_ID, pLogit)
    results <- results[order(-results$pLogit),]
    #results <- subset(results,pLogit >= 0.3,)
    results$pLogit <- paste0(round(results$pLogit*100,0),"%")
    colnames(results) <- c("Employee_ID","Probability of Leaving")
    #cm <- table(results$yLogit, results$y)
    
    return(results)
  })
}

#ShinyApp component
shinyApp(ui = ui, server = server)