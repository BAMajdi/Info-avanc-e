library(shiny)
library(shinydashboard)
library(shinythemes)
library(readxl)
library(ggplot2)
library(scales)
library(dplyr)
library(plotly)
library(corrplot)
library(caret)
library(ROCR)
library(pROC)
library(rpart)
library(rpart.plot)
library(randomForest)
library(nnet)
library(NeuralNetTools)
library(e1071)
library(rsconnect)
library(namespace)


# dataquali=data1()[,a()]
# dataquanti=data1()[,b()]
#train and test

# #modele logistique
# summary(LogisticModel)
#



#
#
#


# data1()=data1() %>% mutate_if(is.character, as.factor)
set.seed(12420)
shinyServer(function(input, output)  {
    
    data1<- reactive({
        inFile <- input$csv_file1
        if (is.null(inFile)){
            return(NULL)}
        data <- read_excel(inFile$datapath )
        
        
        data
    })
    
    output$filetable= DT::renderDataTable({
        data1()
        
        
    }, options = list(scrollX = TRUE))
    
    data2_train<-reactive({
        in.train <- createDataPartition(data1()[["DEATH_EVENT"]], p=0.7, list=FALSE)
        data2_train <- data1()[in.train,]
        
    })
    data2_test<-reactive({
        in.train <- createDataPartition(data1()[["DEATH_EVENT"]], p=0.7, list=FALSE)
        
        
        
        
        data2_test=data1()[-in.train,]
    })
    
    LogisticModel <- reactive({
        
        glm( DEATH_EVENT~.,family = binomial(link = logit),data = data2_train())
    })
    
    a<-reactive({
        c("anaemia","diabetes","high_blood_pressure","sex","smoking","DEATH_EVENT")
    }) 
    b<-reactive({c("age","creatinine_phosphokinase","ejection_fraction","platelets","serum_creatinine","serum_sodium","time")
    })
    output$quali<-renderUI({
        selectInput("quali","Choisir une variable Qualitative",choices=a())  
    })
    output$quali1<-renderUI({
        selectInput("bi2","Choisir une variable Qualitative",choices=a())  
    })
    output$quanti<-renderUI({
        selectInput("quanti","Choisir une variable Quantitative",choices=b())  
    })
    output$quanti1<-renderUI({
        selectInput("bi1","Choisir une variable Quantitative",choices=b())  
    })
    output$plot1<-renderPlot({
        x10 = data1()[,input$quali]
        
        
        
        basex=as.data.frame( table(x10))
        colnames(basex)=c("group","value")
        value=basex$value
        bp<- ggplot(basex, aes(x="", y=value, fill=group))+
            geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+labs(fill="group")
        blank_theme <- theme_minimal()+
            theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.border = element_blank(),
                panel.grid=element_blank(),
                axis.ticks = element_blank(),
                plot.title=element_text(size=14, face="bold"),
                legend.title = element_text("group")
            )
        
        
        bp +  blank_theme +
            theme(axis.text.x=element_blank())+
            geom_text(aes(y = value/2+ c( value[2],0), 
                          label = round( value/sum(value) *100, digits = 2)), size=5)+
            title(names(x10))
    })
    
    output$plot2<-renderPlotly({
        data=data1()
        ggplotly(ggplot(data,aes(y=data[[input$quanti]]))+geom_boxplot()+ylab(input$quanti))
    })
    
    output$statdes=renderText({summary(data1()[,input$quanti])})
    
    output$boxplot1<-renderPlotly({
        
        
        ggplotly( ggplot(data1(),aes(y=data1()[[input$bi1]],x=as.factor(DEATH_EVENT)))+geom_boxplot(aes(fill=as.factor(DEATH_EVENT)))+
                      xlab("DEATH EVENT")+ylab(input$bi)+scale_fill_hue("DEATH EVENT"))
        
    })
    
    output$test1<-renderTable({
        x1 <- unlist(data1()[,input$bi1])
        unlist(t.test(x1,data1()$DEATH_EVENT))
    } ,rownames = TRUE)
    
    
    
    
    output$plot3=renderPlot({
        x2 = unlist(data1()[,input$bi2])
        mosaicplot(table(x2,data1()$DEATH_EVENT), shade = TRUE,color = TRUE,ylab = "DEATH_EVENT" )
        
    })
    
    
    output$test2<-renderTable({
        x2 <- unlist(data1()[,input$bi2])
        unlist(fisher.test(x2,data1()$DEATH_EVENT))
    } ,rownames = TRUE)
    
    output$test3<-renderTable({
        x2 <- unlist(data1()[,input$bi2])
        unlist(chisq.test(x2,data1()$DEATH_EVENT))
    } ,rownames = TRUE)
    
    
    output$corquanti<-renderPlot({cor1<-cor(data1()[,b()])
    corrplot(cor1, type="upper", order="hclust", tl.col="black", tl.srt=45)})
    output$corquali<-renderPlot({cor2<-cor(data1()[,a()])
    corrplot(cor2, type="upper", order="hclust", tl.col="black", tl.srt=45)})
    
    output$boxplot5<-renderPlot({
        set.seed(12420)
        fitLog <- predict(LogisticModel(),type="response",data2_test())
        predlogit = prediction( fitLog, data2_test()$DEATH_EVENT)
        perflogit <- ROCR::performance(predlogit, "tpr", "fpr")
        plot(perflogit, lwd=2, colorize=TRUE, main="ROC : Logistic Regression Performance")
        lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
        lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)
        
        
    })
    output$auc1=renderText({
        set.seed(12420)
        fitLog <- predict(LogisticModel(),type="response",data2_test())
        predlogit = prediction( fitLog, data2_test()$DEATH_EVENT)
        
        AUCLog2<- ROCR::performance(predlogit, measure = "auc")@y.values[[1]]
        
        paste("ROC_AUC: ", AUCLog2 )})
    output$mat1=renderPrint({
        set.seed(12420)
        fitLog <- predict(LogisticModel(),type="response",data2_test())
        predlogit = prediction( fitLog, data2_test()$DEATH_EVENT)
        perflogit <- ROCR::performance(predlogit, "tpr", "fpr")
        predicted.classes <- ifelse(fitLog > 0.5, "1", "0")
        
        conf1<-confusionMatrix(as.factor(predicted.classes),
                               as.factor( data2_test()$DEATH_EVENT),positive="1")
        conf1
    })
    output$plot6<-renderPlot({
        set.seed(12420)
        ArbreModel<- rpart(DEATH_EVENT ~ .,data = data2_train())
        
        fitArbre <- predict(ArbreModel,newdata=data2_test())
        predarbre = prediction(fitArbre, data2_test()$DEATH_EVENT)
        perfarbre <-ROCR:: performance(predarbre, "tpr", "fpr")
        prp(ArbreModel,type=2,extra=1)
        
    })
    
    
    output$plot7<-renderPlot({
        set.seed(12420)
        ArbreModel<- rpart(DEATH_EVENT ~ .,data = data2_train())
        
        fitArbre <- predict(ArbreModel,newdata=data2_test())
        predarbre = prediction(fitArbre, data2_test()$DEATH_EVENT)
        perfarbre <-ROCR:: performance(predarbre, "tpr", "fpr")
        plot(perfarbre, lwd=2, colorize=TRUE, main="ROC : Decision Tree Performance")
        lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
        lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)
    })
    output$auc2=renderText({
        set.seed(12420)
        ArbreModel<- rpart(DEATH_EVENT ~ .,data = data2_train())
        
        fitArbre <- predict(ArbreModel,newdata=data2_test())
        predarbre = prediction(fitArbre, data2_test()$DEATH_EVENT)
        
        AUCArbre=ROCR::performance(predarbre, measure = "auc")@y.values[[1]]
        paste("ROC_AUC: ",AUCArbre)})
    output$mat2=renderPrint({
        ArbreModel<- rpart(DEATH_EVENT ~ .,data = data2_train())
        fitArbre <- predict(ArbreModel,newdata=data2_test())
        predicted.classes <- ifelse(fitArbre > 0.5, "1", "0")
        
        conftarbre<-caret::confusionMatrix(as.factor(predicted.classes),as.factor( data2_test()$DEATH_EVENT),positive="1")
        
        conftarbre})
    
    output$plot8<-renderPlot({
        set.seed(12420)
        RF <- randomForest(DEATH_EVENT~., data = data2_train())
        fitForet <- predict(RF,newdata=data2_test())
        predforet = prediction( fitForet, data2_test()$DEATH_EVENT)
        
        perfrandomforest <-ROCR:: performance(predforet, "tpr", "fpr")
        plot(perfrandomforest, lwd=2, colorize=TRUE, main="ROC : Random Forest Performance")
        lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
        lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)})
    
    output$auc3<-renderText({
        set.seed(12420)
        RF <- randomForest(DEATH_EVENT~., data = data2_train())
        fitForet <- predict(RF,newdata=data2_test())
        predforet = prediction(fitForet, data2_test()$DEATH_EVENT)
        
        AUCRF=ROCR::performance(predforet, measure = "auc")@y.values[[1]]
        paste("ROC_AUC: ",AUCRF)})
    output$mat3=renderPrint({
        RF <- randomForest(DEATH_EVENT~., data = data2_train())
        fitForet <- predict(RF,newdata=data2_test())
        predicted.classes <- ifelse(fitForet > 0.5, "1", "0")
        
        conftRF<-caret::confusionMatrix(as.factor(predicted.classes),as.factor( data2_test()$DEATH_EVENT),positive="1")
        conftRF})
    
    output$plot15<-renderPlot({
        set.seed(12420)
        Neural<- nnet(DEATH_EVENT~ .,data = data2_train(),size=20,maxit=10000,decay=.001, linout=F, trace = F)
        fitNeural <- predict(Neural,
                             newdata=data2_test())
        prednn = prediction( fitNeural, data2_test()$DEATH_EVENT)
        perfnn <-ROCR:: performance(prednn, "tpr", "fpr")
        plot(perfnn, lwd=2, colorize=TRUE, main="ROC : Neural Network Performance")
        lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
        lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)
        
    })
    
    output$auc4<-renderText({
        set.seed(12420)
        Neural<- nnet(DEATH_EVENT~ .,data = data2_train(),size=20,maxit=10000,decay=.001, linout=F, trace = F)
        fitNeural <- predict(Neural,
                             newdata=data2_test())
        prednn = prediction( fitNeural, data2_test()$DEATH_EVENT)
        AUCnn=ROCR::performance(prednn, measure = "auc")@y.values[[1]]
        
        
        
        paste("ROC_AUC: ",AUCnn)})
    output$mat4=renderPrint({
        set.seed(12420)
        Neural<- nnet(DEATH_EVENT~ .,data = data2_train(),size=20,maxit=10000,decay=.001, linout=F, trace = F)
        fitNeural <- predict(Neural,
                             newdata=data2_test())
        
        predicted.classes <- ifelse(fitNeural > 0.5, "1", "0")
        
        conftRN<-confusionMatrix(as.factor(predicted.classes),as.factor( data2_test()$DEATH_EVENT),positive="1")
        conftRN
    })
    
    output$plot9<-renderPlot({
        set.seed(12420)
        fitLog <- predict(LogisticModel(),type="response",data2_test())
        predlogit = prediction( fitLog, data2_test()$DEATH_EVENT)
        perflogit <- ROCR::performance(predlogit, "tpr", "fpr")
        
        Neural<- nnet(DEATH_EVENT~ .,data = data2_train(),size=20,maxit=10000,decay=.001, linout=F, trace = F)
        fitNeural <- predict(Neural,
                             newdata=data2_test())
        prednn = prediction( fitNeural, data2_test()$DEATH_EVENT)
        perfnn <-ROCR:: performance(prednn, "tpr", "fpr")
        RF <- randomForest(DEATH_EVENT~., data = data2_train())
        fitForet <- predict(RF,newdata=data2_test())
        predforet = prediction(fitForet, data2_test()$DEATH_EVENT)
        
        ArbreModel<- rpart(DEATH_EVENT ~ .,data = data2_train())
        
        fitArbre <- predict(ArbreModel,newdata=data2_test())
        predarbre = prediction(fitArbre, data2_test()$DEATH_EVENT)
        perfarbre <-ROCR:: performance(predarbre, "tpr", "fpr")
        perfrandomforest <-ROCR:: performance(predforet, "tpr", "fpr")
        
        plot(perflogit, col='blue', lty=1, main='ROCs: Model Performance Comparision') 
        plot(perfrandomforest, col='red',add=TRUE,lty=4); 
        plot(perfnn, col='black',add=TRUE,lty=8);
        plot(perfarbre, col='green',add=TRUE,lty=9); 
        legend(0.6,0.5,
               c('Logistic Regression ', 
                 'Random fForest', "Neural Network", 
                 "Decision Tree"),
               col=c('blue','red', 'black','green'),
               lwd=3);
        lines(c(0,1),c(0,1),col = "gray", lty = 4 )
    })
    
    output$table<-renderTable({
        set.seed(12420)
        fitLog <- predict(LogisticModel(),type="response",data2_test())
        predlogit = prediction( fitLog, data2_test()$DEATH_EVENT)
        perflogit <- ROCR::performance(predlogit, "tpr", "fpr")
        
        Neural<- nnet(DEATH_EVENT~ .,data = data2_train(),size=20,maxit=10000,decay=.001, linout=F, trace = F)
        fitNeural <- predict(Neural,
                             newdata=data2_test())
        prednn = prediction( fitNeural, data2_test()$DEATH_EVENT)
        perfnn <-ROCR:: performance(prednn, "tpr", "fpr")
        set.seed(12420)
        RF <- randomForest(DEATH_EVENT~., data = data2_train())
        fitForet <- predict(RF,newdata=data2_test())
        predforet = prediction(fitForet, data2_test()$DEATH_EVENT)
        
        ArbreModel<- rpart(DEATH_EVENT ~ .,data = data2_train())
        
        fitArbre <- predict(ArbreModel,newdata=data2_test())
        predarbre = prediction(fitArbre, data2_test()$DEATH_EVENT)
        perfarbre <-ROCR:: performance(predarbre, "tpr", "fpr")
        perfrandomforest <-ROCR:: performance(predforet, "tpr", "fpr")
        
        predicted.classesnn <- ifelse(fitNeural > 0.5, "1", "0")
        AUCnn=performance(prednn, measure = "auc")@y.values[[1]]
        
        predicted.classesrf <- ifelse(fitForet > 0.5, "1", "0")
        AUCRF=ROCR::performance(predforet, measure = "auc")@y.values[[1]]
        
        predicted.classesar <- ifelse(fitArbre > 0.5, "1", "0")
        AUCArbre=ROCR::performance(predarbre, measure = "auc")@y.values[[1]]
        
        AUCLog2<- ROCR::performance(predlogit, measure = "auc")@y.values[[1]]
        predicted.classes <- ifelse(fitLog > 0.5, "1", "0")
        
        conf1<-confusionMatrix(as.factor(predicted.classes),as.factor( data2_test()$DEATH_EVENT),positive="1")
        z=as.data.frame(conf1$byClass)
        ACLOGIT=conf1$overall[1]
        recallLOGIT=z[6,]
        conftarbre<-caret::confusionMatrix(as.factor(predicted.classesar),as.factor( data2_test()$DEATH_EVENT),positive="1")
        m=as.data.frame(conftarbre$byClass)
        recallAR=m[6,]
        ACAR=conftarbre$overall[1]
        conftRF<-confusionMatrix(as.factor(predicted.classesrf),as.factor( data2_test()$DEATH_EVENT),positive="1")
        j=as.data.frame(conftRF$byClass)
        ACRF=conftRF$overall[1]
        recallRF=j[6,]
        conftRN<-caret::confusionMatrix(as.factor(predicted.classesnn),as.factor( data2_test()$DEATH_EVENT),positive="1")
        o=as.data.frame(conftRN$byClass)
        ACRN=conftRN$overall[1]
        recallRN=o[6,]
        models <- c('Logistic regression', 'Random Forest', 'Decision Tree','Neural Network')
        models_AUC <- c(AUCLog2, AUCRF, AUCArbre, AUCnn)
        models_PRECISION<-c(ACLOGIT,ACRF,ACAR,ACRN)
        models_recall<-c(recallLOGIT,recallRF,recallAR,recallRN)
        model_performance <- as.data.frame(cbind(models, models_AUC,models_PRECISION,models_recall))
        colnames(model_performance) <- c("Model", "ROC_AUC","Accuracy","Recall")
        model_performance
    })
    
    
    
    
    
    
})
