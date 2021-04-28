
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plyr)
library(dplyr)
library(data.table)
library(DT)
library(ggplot2)
library(tidyr)
library(naniar)
library(caret)
library(glmnet)
library(corrplot)
options(stringsAsFactors=F)

function(input, output, clientData, session){
  waves=reactive({
    data.frame(wave=c(1:10),
                   startDate=as.Date(c('2020-04-06','2020-04-15','2020-05-21','2020-09-08','2020-11-24',
                                       '2020-12-19','2021-01-12','2021-02-08',NA,'2021-03-02')),
                   endDate=as.Date(c('2020-04-14','2020-05-20','2020-09-07','2020-11-23','2020-12-18',
                                     '2021-01-11','2021-02-07','2021-03-01',NA,'2021-04-28')))
  })
  
  data=reactive({
    readRDS('covid_fbsurveyDat_aggr.rds')
    })
  
  output$waveDat = renderDT({
    datatable(waves(), 
              escape = FALSE,
              style = "bootstrap",
              rownames=F,
              options = list(dom = 't',
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}")))
  })
  
  res.tab=reactive({
    w=as.numeric(input$wave)
    start=waves()$startDate[w]
    end=waves()$endDate[w]
    
    # remove observations with >90% missing values
    res.tab=data() %>% filter(time_value<=end & time_value>=start) 
    colnames(res.tab)=gsub('value\\+0\\:fb\\-survey_smoothed_','',colnames(res.tab))
    # further remove variables with >20% missing values 
    # and observations with missing values in response variables
    wws=colnames(res.tab) %>% .[grepl('^ww',.)] 
    wws=gsub('ww','w',wws)
    res.tab = res.tab %>% select(geo_value,time_value,starts_with('w')) %>% select(-wws)
    
    ind_c=colSums(!is.na(res.tab),na.rm = T)/nrow(res.tab)>0.8
    res.tab=res.tab[,ind_c]
    
    cols=colnames(res.tab)[grep('wdepressed|wanxious',colnames(res.tab))]
    res.tab=res.tab[!apply(res.tab[,cols],1,anyNA),]
    
    res.X = res.tab %>% 
      select(-contains(c('anxious','depressed')),-geo_value,-time_value)
    rownames(res.X)=paste0(res.tab$geo_value,'_',res.tab$time_value)
    res.Y.depressed=res.tab[,colnames(res.tab)[grep('wdepressed',colnames(res.tab))]]
    res.Y.anxious=res.tab[,colnames(res.tab)[grep('wanxious',colnames(res.tab))]]
    res.tab.impmod = preProcess(res.X,method = 'medianImpute')
    res.tab.imp = predict(res.tab.impmod, res.X) 
    
    list(res.tab=res.tab,res.tab.imp=res.tab.imp,
         res.Y.depressed=res.Y.depressed,res.Y.anxious=res.Y.anxious)
  })
  
  
  output$vismiss=renderPlot({
    # visualize missing data
    vis_miss(res.tab()$res.tab[sample(1:nrow(res.tab()$res.tab),1000,replace = F),],warn_large_data=F)+
      theme(axis.text.x = element_text(size=12,angle=90))
  })
  
  output$corrplot=renderPlot({
    # visualize correlations
    corMat = cor(res.tab()$res.tab.imp)
    corrplot(corMat)
  })
  
  
  enet=reactive({
    # split data into training and testing datasets
    set.seed(1)
    trainIndex = createDataPartition(res.tab()$res.Y.depressed, 
                                     p = .7, list = FALSE) %>% as.vector(.)
    testIndex = which(!(1:nrow(res.tab()$res.tab.imp) %in% trainIndex))
    Xtrain = res.tab()$res.tab.imp[trainIndex,] %>% as.matrix()
    Ytrain_d = res.tab()$res.Y.depressed[trainIndex]
    Ytrain_a = res.tab()$res.Y.anxious[trainIndex]
    
    Xtest = res.tab()$res.tab.imp[testIndex,] %>% as.matrix()
    Ytest_d = res.tab()$res.Y.depressed[testIndex]
    Ytest_a = res.tab()$res.Y.anxious[testIndex]
    
    # train the model and 25-fold CV
    set.seed(1)
    K = 25
    trainControl = trainControl(method = "cv", number = K)
    elasticOut_d = train(x = Xtrain, y = Ytrain_d,
                       method = "glmnet", trControl = trainControl)
    elasticOut_a = train(x = Xtrain, y = Ytrain_a,
                         method = "glmnet", trControl = trainControl)
    list('elasticOut_a'=elasticOut_a,
         'elasticOut_d'=elasticOut_d,
         'Xtrain'=Xtrain,'Ytrain_d'=Ytrain_d,'Ytrain_a'=Ytrain_a,
         'Xtest'=Xtest,'Ytest_d'=Ytest_d,'Ytest_a'=Ytest_a,'p'=p)
  })
  

  
  output$enetPlot_d=renderPlot({
    plot(enet()$elasticOut_d)
  })
  
  output$tuneDat_d = renderDT({
    datatable(data.frame(alpha=enet()$elasticOut_d$bestTune$alpha,
                         lambda=enet()$elasticOut_d$bestTune$lambda), 
              escape = FALSE,
              style = "bootstrap",
              rownames=F,
              options = list(dom = 't',
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}")))
  })
  
  output$enetPlot_a=renderPlot({
    plot(enet()$elasticOut_a)
  })
  
  output$tuneDat_a = renderDT({
    datatable(data.frame(alpha=enet()$elasticOut_a$bestTune$alpha,
                         lambda=enet()$elasticOut_a$bestTune$lambda), 
              escape = FALSE,
              style = "bootstrap",
              rownames=F,
              options = list(dom = 't',
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}")))
  })
  
  output$featurePlot_d=renderPlot({
    # refit the model with best-performing parameters
    glmnetOut  = glmnet(x = enet()$Xtrain, y = enet()$Ytrain_d, alpha = enet()$elasticOut_d$bestTune$alpha)
    betaHat = coef(glmnetOut, s = enet()$elasticOut_d$bestTune$lambda)

    # predict training data
    YhatTrain_glmnet = predict(glmnetOut, enet()$Xtrain,  s = enet()$elasticOut_d$bestTune$lambda)
    # plot(YhatTrain_glmnet, Ytrain - YhatTrain_glmnet,
    #      xlab = 'Training predictions', ylab = 'Residuals')
    trainError=mean((enet()$Ytrain_d - YhatTrain_glmnet)**2)

    # predict testing data
    YhatTest_glmnet = predict(glmnetOut, enet()$Xtest,  s = enet()$elasticOut_d$bestTune$lambda)
    testError=mean((enet()$Ytest_d - YhatTest_glmnet)**2)

    plotDat = data.frame(Features=names(betaHat[-1,]), Beta=betaHat[-1,]) %>%
      filter(Beta!=0) %>%
      arrange(desc(abs(Beta))) %>%
      mutate(Direction=ifelse(Beta>0,'positive','negative'))

    ggplot(plotDat, aes(x=reorder(Features,abs(Beta)), y=Beta,color=Direction)) +
      geom_segment(aes(x=reorder(Features,abs(Beta)),
                       xend=reorder(Features,abs(Beta)),
                       y=0, yend=Beta)) +
      geom_point(size=4, alpha=0.6) +
      scale_color_manual(values=c('blue','red'))+
      theme_light() +
      coord_flip() +
      theme(legend.position = 'none',
            panel.grid.major.y = element_blank(),
            panel.border = element_blank(),
            axis.text = element_text(size=11),
            axis.title = element_text(size=13,face='bold'))+
      xlab('Features')+
      ggtitle(paste0('Training Error = ',round(trainError,3),', Testing Error = ',round(testError,3)))

  })
  
  output$featurePlot_a=renderPlot({
    # refit the model with best-performing parameters
    glmnetOut  = glmnet(x = enet()$Xtrain, y = enet()$Ytrain_a, alpha = enet()$elasticOut_a$bestTune$alpha)
    betaHat = coef(glmnetOut, s = enet()$elasticOut_a$bestTune$lambda)
    
    # predict training data
    YhatTrain_glmnet = predict(glmnetOut, enet()$Xtrain,  s = enet()$elasticOut_a$bestTune$lambda)
    # plot(YhatTrain_glmnet, Ytrain - YhatTrain_glmnet,
    #      xlab = 'Training predictions', ylab = 'Residuals')
    trainError=mean((enet()$Ytrain_a - YhatTrain_glmnet)**2)
    
    # predict testing data
    YhatTest_glmnet = predict(glmnetOut, enet()$Xtest,  s = enet()$elasticOut_a$bestTune$lambda)
    testError=mean((enet()$Ytest_a - YhatTest_glmnet)**2)
    
    plotDat = data.frame(Features=names(betaHat[-1,]), Beta=betaHat[-1,]) %>%
      filter(Beta!=0) %>%
      arrange(desc(abs(Beta))) %>%
      mutate(Direction=ifelse(Beta>0,'positive','negative'))
    
    ggplot(plotDat, aes(x=reorder(Features,abs(Beta)), y=Beta,color=Direction)) +
      geom_segment(aes(x=reorder(Features,abs(Beta)),
                       xend=reorder(Features,abs(Beta)),
                       y=0, yend=Beta)) +
      geom_point(size=4, alpha=0.6) +
      scale_color_manual(values=c('blue','red'))+
      theme_light() +
      coord_flip() +
      theme(legend.position = 'none',
            panel.grid.major.y = element_blank(),
            panel.border = element_blank(),
            axis.text = element_text(size=11),
            axis.title = element_text(size=13,face='bold'))+
      xlab('Features')+
      ggtitle(paste0('Training Error = ',round(trainError,3),', Testing Error = ',round(testError,3)))
    
  })
  
  output$dict = renderDT({
    w=as.numeric(input$wave)
    dat=fread('dictionary.csv')
    if(w==1){w='^1,'}
    
    datatable(dat %>% filter(grepl(w,waves)), 
              escape = FALSE,
              style = "bootstrap",
              rownames=F,
              options = list(dom = 't',
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}")))
  })
}
