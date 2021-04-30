library(shiny) 
library(DAAG)
library(corrplot)
library(ggplot2)
library(reshape2)
library(magrittr)
library(dplyr)
library(ROSE)
library(DMwR)
library(caret) # for sampling
library(caTools) # for train/test split
library(rpart)# for decision tree model
library(Rborist)# for random forest model
library(rpart)
library(rpart.plot)
creditcard = read.csv('creditcard.csv',sep=',',header = TRUE)
credcard=creditcard
credcard$hours <- round(credcard$Time / 3600)
credcard$hours24 <- credcard$hours %% 24
fig <- function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)
}

#################Splitting and scaling data
df <- creditcard[,-1]
#Change 'Class' variable to factor
df$Class <- as.factor(df$Class)
levels(df$Class) <- c("Not_Fraud", "Fraud")
#Scale numeric variables
df[,-30] <- scale(df[,-30])

set.seed(123)
split <- sample.split(df$Class, SplitRatio = 0.7)
train <-  subset(df, split == TRUE)
test <- subset(df, split == FALSE)
# smote
set.seed(9560)
smote_train <- SMOTE(Class ~ ., data  = train)
#CART Model Performance on imbalanced data
set.seed(5627)
orig_fit <- rpart(Class ~ ., data = train)
#Evaluate model performance on test set
pred_orig <- predict(orig_fit, newdata = test, method = "class")
smote_fit <- rpart(Class ~ ., data = smote_train)
set.seed(5627)
pred_orig1 <- predict(smote_fit, newdata = test, method = "class")
#LG with blanced data
glm_fit1 <- glm(Class ~ ., data = smote_train, family = 'binomial')
pred_glm1 <- predict(glm_fit1, newdata = test, type = 'response')
#LG with imbalanced data
glm_fit <- glm(Class ~ ., data = train, family = 'binomial')
pred_glm <- predict(glm_fit, newdata = test, type = 'response')
tree <- rpart(test$Class~., data=test, method = 'class', model=TRUE, cp=0.01)
time_of_day <- creditcard$Time %%(24*3600)

# User Interface
ui <- fluidPage(
  titlePanel("Credit Card Fraud Detection Shiny app"),
  h4("MINI PROJET DATA2: RIDHA TIGOULMAMINE & HOUCINE FORLOUL."),
  sidebarLayout(
    sidebarPanel(
      selectInput("select", label = h4("Select a feature for univariate analysis"),
                  choices = names(creditcard), 
                  selected = 1),
      selectInput("selectB", label = h4("Select a feature for bivariate analysis"),
                  choices = (names(creditcard)), 
                  selected = 1),
      h1("Résumé:"),
      h5("Les ensembles de données contiennent des transactions effectuées par carte de crédit par des titulaires de carte européens. Cet ensemble de données présente les transactions qui ont eu lieu en deux jours, où nous avons 492 fraudes sur 284 807 transactions. L'ensemble de données est très déséquilibré, la classe positive (fraudes) représente 0,172% de toutes les transactions."),
      h5("Il contient uniquement des variables d'entrée numériques qui sont le résultat d'une transformation PCA. Malheureusement, en raison de problèmes de confidentialité, nous ne pouvons pas fournir les fonctionnalités d'origine et plus d'informations générales sur les données. Les fonctionnalités V1, V2,… V28 sont les principaux composants obtenus avec PCA, les seules fonctionnalités qui n'ont pas été transformées avec PCA sont 'Time' et 'Amount'. La fonction «Temps» contient les secondes écoulées entre chaque transaction et la première transaction de l'ensemble de données. La fonction «Montant» est le montant de la transaction, cette fonction peut être utilisée pour un apprentissage dépendant des coûts, par exemple. La caractéristique «Classe» est la variable de réponse et prend la valeur 1 en cas de fraude et 0 dans le cas contraire."),
      h5("L'ensemble de données est très déséquilibré, le compte de classe positive (fraudes) ne représente qu'environ 0,172% des 284807 transactions."),
      ),
    mainPanel(
      tabsetPanel(
        tabPanel("L’analyse univariéee", 
                 fluidRow(
                  column(6, 
                          plotOutput(outputId = "boxplot")),
                  column(6, 
                          plotOutput(outputId = "effectifsHist1")),
                  column(6,align="center",
                         div(style = "height:50px;width:100%"),
                         tableOutput("statsTableOut")),
                  column(6,align="center",
                          plotOutput(outputId = "eff_ed")),
                          )),
        tabPanel("L’analyse bivariate",
                 fluidRow(
                   column(6, 
                          plotOutput(outputId = "Nuage")),
                   column(6, 
                          plotOutput(outputId = "Correlation1"))
                 )),
        tabPanel("Fraud attrition",
                 fluidRow(
                   column(6, 
                          plotOutput(outputId = "plot1")),
                   column(6, 
                          plotOutput(outputId = "plot2"))
                 ),
                 fluidRow(
                   column(6, 
                          plotOutput(outputId = "plot3")),
                   column(6, 
                          plotOutput(outputId = "plot4"))
                 )),
        tabPanel("Apprentissage automatique",
                   fluidRow( h3("Arbre de décision avec les données déséquilibré",align="center"),
                     column(6,
                            plotOutput(outputId = "ROCcurve")),
                     column(6,
                            plotOutput(outputId = "CARTvis")),
                            ),
                   fluidRow(h3("Arbre de décision avec les données rééquilibré",align="center"),
                   column(6,
                          plotOutput(outputId = "ROCcurve1")),
                   column(6,
                          plotOutput(outputId = "CARTvis2")),
                   fluidRow(h3("Régression Logistique avec les données déséquilibré puis avec les données rééquilibré",align="center"),
                            column(6,
                                   plotOutput(outputId = "ROC_RL")),
                            column(6,
                                   plotOutput(outputId = "ROC_RL1")),
                   ),
                   )),
        tabPanel("Table", dataTableOutput("table"), style = "font-size: 85%")
      )
    )
  )
)

# Serveur
server <- function(input, output) {
  output$table <- renderDataTable({creditcard})
  # Tableau statistique [quantitative]
  tabStatsQuant <- reactive({
    q = data.frame(statistiques = c('min', 'quantile 25%', 'median', 'quantile 75%',
                                    'max', 'moyenne', 'ecart type'),
                   values = c(quantile(creditcard[, input$select]), 
                              mean(creditcard[, input$select]),
                              sd(creditcard[, input$select]))
    )
  });
  output$statsTableOut <- renderTable({ 
    if (is.numeric(creditcard[, input$select]))
    {
      tabStatsQuant()
    } else {
      tabStatsQual() 
    }
  })
  output$effectifsHist1 <- renderPlot({
    ggplot2::qplot(creditcard[, input$select],fill=creditcard$Class,color = "#C4961A",
    main = paste("Histogramme de", input$select, sep=" "),
    xlab = paste("Indice de", input$select, sep=" "), ylab = "")
  })
  output$Nuage <- renderPlot({
    p <- ggplot(creditcard, aes(x=creditcard[, input$select], y=creditcard[, input$selectB])) + geom_point()
    p + labs(x = input$select,y = input$selectB)
    
  })
  output$boxplot <- renderPlot({
    if(! is.numeric(creditcard[, input$select])) return(NULL)
    boxplot(creditcard[, input$select], main=paste("Boxplot de", input$select, sep=" "),col= '#00AFBB')
  })
  output$ROCcurve <- renderPlot({
    roc.curve(test$Class, pred_orig[,2], plotit = TRUE, n.thresholds=100)
  })
  output$ROCcurve1 <- renderPlot({
  roc.curve(test$Class, pred_orig1[,2], plotit = TRUE)
  })
  output$CARTvis2 <- renderPlot({
    rpart.plot(smote_fit, box.palette="RdBu", shadow.col="gray")
  })
  output$CARTvis <- renderPlot({
    rpart.plot(tree, box.palette="RdBu", shadow.col="gray")
  })
  output$ROC_RL1 <- renderPlot({
  roc.curve(test$Class, pred_glm1, plotit = TRUE)
  })
  output$ROC_RL <- renderPlot({
    roc.curve(test$Class, pred_glm, plotit = TRUE)
  })
  output$eff_ed <- renderPlot({
    if(! is.numeric(creditcard[, input$select])) return(NULL)
    tmp.hist <- hist( creditcard[, input$select], plot = FALSE,
                        right = FALSE)
    plot(x = tmp.hist$breaks[-1], y = cumsum(tmp.hist$counts),
         xlab = input$select,
         ylab = "cumules",
         main = paste("Courbe cumulative de ", input$select, sep=""),
         type = "o", col = "#00AFBB", lwd = 2)
  })
  ###############churn
  output$plot1 <- renderPlot({
  creditcard%>%
    select(Amount,Class)%>%
    ggplot(aes(Amount,fill=as.factor(Class)))+
    geom_histogram()+
    facet_wrap(~Class,scales='free_y',ncol=1)+
    scale_x_log10(label=scales::dollar_format())+
    labs(title="Fraud by amount spent",fill="Fraud")
  })
  output$plot2 <- renderPlot({
  fig(12, 8)
  common_theme <- theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  ggplot(data = creditcard, aes(x = factor(Class), 
                        y = prop.table(stat(count)), fill = factor(Class),
                        label = scales::percent(prop.table(stat(count))))) +
    geom_bar(position = "dodge") + 
    geom_text(stat = 'count',
              position = position_dodge(.9), 
              vjust = -0.5, 
              size = 3) + 
    scale_x_discrete(labels = c("no fraud", "fraud"))+
    scale_y_continuous(labels = scales::percent)+
    labs(x = 'Class', y = 'Percentage') +
    ggtitle("Distribution of class labels") +
    common_theme
  })
  output$plot3 <- renderPlot({
  fig(14, 8)
  creditcard %>%
    ggplot(aes(x = Time, fill = factor(Class))) + geom_histogram(bins = 100)+
    labs(x = 'Time in seconds since first transaction', y = 'No. of transactions') +
    ggtitle('Distribution of time of transaction by class') +
    facet_grid(Class ~ ., scales = 'free_y') 
  })
  output$plot4 <- renderPlot({
  credcard %>%
    mutate(period = round(Time / 3600)) %>%
    group_by(period) %>%
    ggplot(aes(hours))+
    geom_histogram(aes(y=..density.., fill = as.factor(Class), color = as.factor(Class)), alpha = 0.5, position = "identity") +
    scale_fill_manual(values = c("lightblue", "pink"), labels = c("Legitimate", "Fraud"), name = "Fraud / Legitimate") +
    scale_color_manual(values = c("blue", "red"), labels = c("Legitimate", "Fraud"), name = "Fraud / Legitimate") +
    labs(title = "Fraud / Legitimate transaction repartition per hour") + 
    theme(plot.title = element_text(hjust = 0.5))
  })
  output$Correlation1 <- renderPlot({
    fig(14, 8)

    correlations <- cor(creditcard[,-1],method="pearson")
    corrplot(correlations, number.cex = .9, method = "circle", type = "full", tl.cex=0.8,tl.col = "black")
  }) 
}
# Lancement de l'application 
shinyApp(ui = ui, server = server)


