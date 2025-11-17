

# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
# Date 03/07/2024
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rhandsontable)
library(tidyverse)
#library(sjPlot)
library(ggplot2)
library(daewr)
library(emmeans)
library(plotly)
library(DT)
#library(hrbrthemes)
library(viridis)
library(ggpubr)
library(pwr)
library(collapsibleTree)
library(webshot)
webshot::install_phantomjs(force=TRUE)
library(future)
library(parallelly)
library(promises)






## Gráfico de homocedasticidad

# homocedast <- function(datos){
#   mod3 <-lm(response ~ Treatment+Block, datos)
#   est <- mod3$fitted.values
#   pred <- mod3$residuals
#   bd1 <- data.frame(est, pred)
#   p4 <- ggplot(bd1) +
#     aes(x = est, y = pred) +
#     geom_point(size= I(3), col='blue') +
#     geom_hline(yintercept = 0, linetype ='dashed') +
#     labs(x = "Fitted values", y = "Residuals") +
#     ggtitle('Homocedasticity plot') +
#     theme(axis.text = element_text(size = 14),
#           axis.title=element_text(size = 16),
#           plot.title = element_text(hjust = 0.5, size = rel(1.5)))
#   return(p4)
# }

# Supuesto de normalidad para supuestos de bloque
# normalidad <- function(datos){
#   res <- lm(response~Treatment+Block,datos)
#   p2 <- ggplot(datos, aes(sample = res$residuals))+ 
#     stat_qq(size= I(3)) + stat_qq_line(size= I(0.6)) +
#     ggtitle('QQ-Plot') +
#     theme(axis.text = element_text(size = 14),
#           axis.title=element_text(size = 16),
#           plot.title = element_text(hjust = 0.5, size = rel(1.5))) +
#     labs(y="Residuals", x="quantils")
#   return(p2)
# }

# Estadistica descriptiva para bloques
stadis_desc <- function(df){
  
  bar_y <- mean(df$response)
  table <- df %>% 
    group_by(Block) %>%
    summarise(
      n = length(response), 
      media = mean(response), 
      sd = sd(response), 
      min = min(response), 
      max = max(response),
      cv = sd(response)/mean(response)*100)
  return(table)}





##############################################################################
shinyServer(function(input, output, session) {
  
  ###Check inputs ################################################################
  observeEvent(input$sigma, {
    if (!is.numeric(input$sigma) || input$sigma < .01) {
      showModal(
        modalDialog(
          title = "Invalid Input",
          div(
            style = "color:red; font-weight:bold;",
            "The value of sigma must be a real number greater than 0"
          ),
          easyClose = TRUE,
          footer = modalButton("OK")
        )
      )
      
      # Optional: reset input to NA or to a safe value
      updateNumericInput(session,"sigma", value = 3)
      
      # Optional: stop further propagation
      return(NULL)
    }
  }, ignoreInit = FALSE)
  
  observeEvent(input$alfa, {
    if (!is.numeric(input$alfa) || input$alfa < .01 || input$alfa>0.2) {
      showModal(
        modalDialog(
          title = "Invalid Input",
          div(
            style = "color:red; font-weight:bold;",
            "The value of alpha must be a real number between 0.01 and 0.2"
          ),
          easyClose = TRUE,
          footer = modalButton("OK")
        )
      )

      # Optional: reset input to NA or to a safe value
      updateNumericInput(session,"alfa", value = 0.05)

      # Optional: stop further propagation
      return(NULL)
    }
  }, ignoreInit = FALSE)
  
  
  
  observeEvent(input$rows, {
    if (!is.numeric(input$rows) ||input$rows %% 1 !=0 || input$rows < 3 || input$rows>6) {
      showModal(
        modalDialog(
          title = "Invalid Input",
          div(
            style = "color:red; font-weight:bold;",
            "The number of treatments must be an integer from 0 to 6"
          ),
          easyClose = TRUE,
          footer = modalButton("OK")
        )
      )
      
      # Optional: reset input to NA or to a safe value
      updateNumericInput(session,"rows", value = 3)
      
      # Optional: stop further propagation
      return(NULL)
    }
  }, ignoreInit = FALSE)
  
  observeEvent(input$cols, {
    if (!is.numeric(input$cols) ||input$cols %% 1 !=0 || input$cols < 2) {
      showModal(
        modalDialog(
          title = "Invalid Input",
          div(
            style = "color:red; font-weight:bold;",
            "The number of blocks must be an integer greater than 1"
          ),
          easyClose = TRUE,
          footer = modalButton("OK")
        )
      )
      
      # Optional: reset input to NA or to a safe value
      updateNumericInput(session,"cols", value = 3)
      
      # Optional: stop further propagation
      return(NULL)
    }
  }, ignoreInit = FALSE)
  
  observeEvent(input$replicates, {
    if (!is.numeric(input$replicates) ||input$replicates %% 1 !=0 || input$replicates < 1) {
      showModal(
        modalDialog(
          title = "Invalid Input",
          div(
            style = "color:red; font-weight:bold;",
            "The number of blocks must be an integer greater than 0"
          ),
          easyClose = TRUE,
          footer = modalButton("OK")
        )
      )
      
      # Optional: reset input to NA or to a safe value
      updateNumericInput(session,"replicates", value = 3)
      
      # Optional: stop further propagation
      return(NULL)
    }
  }, ignoreInit = FALSE)
  
  ######################################################################################################## End of checking inputs
  ### Calculate treatment effects from Cohen's f
  
  calculate_group_means <- function(f=0.12, sigma_within=3, mu=0, k=3) {
    # Calculate the standard deviation of the group means
    sigma_means <- f * sigma_within
    
    # Total variance of the group means
    variance_means <- sigma_means^2
    
    # Solve for sum of squared deviations
    sum_squared_deviations <- variance_means * k
    
    # Assume symmetric deviations around the mean
    # Deviations will be equally spaced around the overall mean
    # Create deviations symmetrically
    deviations <- seq(-(k - 1) / 2, (k - 1) / 2, length.out = k)
    
    # Scale deviations to match the sum of squared deviations
    scaling_factor <- sqrt(sum_squared_deviations / sum(deviations^2))
    deviations <- deviations * scaling_factor
    
    # Calculate group means
    group_means <- mu + deviations
    
    return(group_means-group_means[1])
  }
  
  ################ Interactive table for treatment effects
  
  input_table_T <- reactive({
    
    #rr <- vals #replicate(input$rows,runif(1,-5,10))
    rr <- calculate_group_means(f=input$fCohen,sigma=input$sigma,k=input$rows) %>% round(2)
    mat <- matrix(rr, ncol = input$rows, nrow = 1)
    res <- c()
    for (j in 1:input$rows) {res <- c(res,paste("Treat(",j,")",sep=''))}
    colnames(mat) <- res  
    
    res = as.data.frame(mat)
    res
    
  })
  
  output$input_table_T <- renderRHandsontable({
    
    rhandsontable(input_table_T())
  })
  
  values_T <- reactiveValues(data=NULL)
  
  observe({
    values_T$data <- hot_to_r(input$input_table_T)
  })

    dT1 <- reactive({
    values_T$data
  })
  
 
  #################### Data generation and block effect
    
    ##This fubction generates the block effects based on the different levels
    
    generate_block_effects <- function(b,
                                       sigma_within,
                                       level = c("null", "low", "medium", "large"),
                                       sd_ratios = c(null = 0.0, low = 0.5, medium = 1.5, large = 3),
                                       use_sample_sd = FALSE) {
      level <- match.arg(level)

      if (b < 1) stop("b must be >= 1")
      if (!level %in% names(sd_ratios)) stop("Level not found in 'sd_ratios' names")

      # Target block SD as a multiple of sigma_within
      sigma_b_target <- as.numeric(sd_ratios[level]) * sigma_within

      # Base mean-zero pattern (equally spaced scores centered at zero)
      # Example for b=4: -1.5, -0.5, 0.5, 1.5
      z <- seq_len(b) - (b + 1) / 2

      # If b == 1, no blocking possible; return 0
      if (b == 1) {
        betas <- 0
        actual_pop_sd <- 0
        actual_samp_sd <- NA_real_
      } else {
        # Choose population vs sample SD for scaling
        s <- if (use_sample_sd) sd(z) else sqrt(mean(z^2))
        # Scale to achieve the target SD
        scale_factor <- if (s > 0) sigma_b_target / s else 0
        betas <- z * scale_factor

        # Diagnostics
        actual_pop_sd <- sqrt(mean((betas - mean(betas))^2))
        actual_samp_sd <- sd(betas)
      }

      # Return with attributes for transparency
      betas
    }

    
  
    ### Generate data using the parameters of the defined scenario 
    ### and block effects
    
  dataBlocks <- function(trat,blocks,repli,error,mean,op){
    
    m <- mean
    s <- error
    
    # nulo  <- runif(blocks, -0.001*m, 0.001*m)
    # bajo  <- runif(blocks, -.1*m, .1*m)
    # medio <- runif(blocks, -.5*m, .5*m)
    # alto  <- runif(blocks, -1*m, 1*m)


#    rest_ran <- as.data.frame(cbind(nulo,bajo,medio,alto))

    if(op == "Nulo") {
      # eF1 <- t(dF1())
      # #eF2 <- t(dF2())
      eBlock <- generate_block_effects(blocks,s,"null")
    }
    else if (op == "Bajo") {
      # eF1 <- t(dF1())
      # #eF2 <- t(dF2())
      eBlock <- generate_block_effects(blocks,s,"low")
    }
    else if (op == "Medio") {
      # eF1 <- t(dF1())
      # #eF2 <- t(dF2())
      eBlock <- generate_block_effects(blocks,s,"medium")
    }
    else {
      # eF1 <- t(dF1())
      # #eF2 <- t(dF2())
      eBlock <- generate_block_effects(blocks,s,"large")
    }

    
   
    
     
    eTreatment <- t(dT1())
    
    data <- data.frame(
      Treatment=rep(rep(1:trat,1),blocks*repli),
      Block=rep(rep(1:blocks),trat) %>% sort()
    )
    k <- trat
    b <- blocks
    r <- repli
    n <- k*b*r
    
    ss1 <- error
    
    
    
    data <- data %>% mutate(response=mean+eTreatment[Treatment]+eBlock[Block]+rnorm(n,0,ss1))
    data$Treatment <- as.factor(data$Treatment)
    data$Block <- as.factor(data$Block)
    
    list(data = data, eBlock = eBlock)
  }
  
  
  
  # Data generation in response to the button go
  
  dataBlocksSim <- eventReactive(input$go,{
    dataBlocks(trat=input$rows,
               blocks=input$cols,
               repli=input$replicates,
               error=input$sigma,
               mean=input$mu,
               op=input$opcion)
  },ignoreNULL = TRUE)
  
 
 ## Structure of the RCBD 
  
  output$design_RCBD <-renderCollapsibleTree({
    d <- dataBlocksSim()
    d <- d$data
    # d$Treatment <- paste0("Treatment: ", d$Treatment)
    # d$Block <- paste0("Block: ", d$Block)
    d$Treatment <- paste0(input$TreatName,":",d$Treatment)
    d$Block <- paste0(input$BlockName,":", d$Block)
    d$response <- d$response %>% round(2)
    
    Design <- d
    collapsibleTree(Design,
                    hierarchy=c('Block','Treatment','response'),
                    collapsed=F)
    
  })
  
  ## Structure of the CRD
  
  output$design_CRD <-renderCollapsibleTree({
    d <- dataBlocksSim()
    d <- d$data
    # d$Treatment <- paste0("Treatment: ", d$Treatment)
    # d$Block <- paste0("Block: ", d$Block)
    d$Treatment <- paste0(input$TreatName,":",d$Treatment)
    d$Block <- paste0(input$BlockName,":", d$Block)
    d$response <- d$response %>% round(2)
    
    Design <- d
    collapsibleTree(Design,
                    hierarchy=c('Treatment','response'),
                    collapsed=F)
    
  })
  
  
  ## Download data corresponding to a simulation (npout used so far)
  
  output$downloadData <- downloadHandler(
    filename = function(){"datos.csv"}, 
    content = function(fname){
      write.csv2(dataBlocksSim(), fname)
    }
  )
  
  

  
  ## Table of means RCBD
  
  output$TableMeansRCBD <- renderTable({
    d <- dataBlocksSim()
    d <- d$data
    d$Block <- factor(d$Block)
    d$Treatment <- factor(d$Treatment)
    if (input$replicates >1) {
      d %>%  group_by(Block,Treatment) %>% 
        summarise(Mean=mean(response),sd=sd(response),n=n(),
                  Min=min(response),Max=max(response))}
    else {d %>%  group_by(Block,Treatment) %>% 
        summarise(Response=response)}
    
  })
  
  ## Table of means CRD
  
  output$TableMeansCRD <- renderTable({
    d <- dataBlocksSim()
    d <- d$data
    d$Block <- factor(d$Block)
    d$Treatment <- factor(d$Treatment)
    if (input$replicates >0) {
      d %>%  group_by(Treatment) %>% 
        summarise(Mean=mean(response),sd=sd(response),n=n(),
                  Min=min(response),Max=max(response))}
    else {d %>%  group_by(Treatment) %>% 
        summarise(Response=response)}
    
  })
  
  ## Plot means RCBD
  
  output$ScaleyRange <- renderUI({
    val <- input$MaxScaleyRange
    sliderInput('yRange','Range for response axes',value=c(-val,val),min=-val,max=val)
  })
  
  
  output$PlotMeansRCBD <- renderPlot({
    d <- dataBlocksSim()
    d <- d$data %>% as.data.frame()
    d$Treatment <- factor(d$Treatment)
    d$Block <- factor(d$Block)
    
    ggplot(d,aes(x = Treatment, y = response,color = Block,group = Block)) +
      stat_summary(fun.data = mean_se, 
                   geom = "pointrange") +
      geom_line(stat = "summary", fun = mean, size=.75, linetype=2)+
      labs(x='Factor',y='Respuesta',color='Blocks')+
      ylim(input$yRange[1],input$yRange[2])+
      labs(x='Treatment',y='Response')
  })
  
  ## Plot means CRD
  
  output$PlotMeansCRD<- renderPlot({
    d <- dataBlocksSim()
    d <- d$data %>% as.data.frame()
    d$Treatment <- factor(d$Treatment)
    d$Block <- factor(d$Block)
    
    ggplot(d,aes(x = Treatment, y = response)) +
      stat_summary(fun.data = mean_se, 
                   geom = "pointrange") +
      ylim(input$yRange[1],input$yRange[2])+
      labs(x='Treatment',y='Response')
  })
  
 
  
  ## RCBD ANOVA Table
  
  output$ANOVA_RCBD <- output$ANOVA_RCBD_bis <- renderPrint({
    d <- dataBlocksSim() 
    d <- d$data %>% as.data.frame()
    d$Treatment <- factor(d$Treatment)
    d$Block <- factor(d$Block)
    res <- lm(response~Block+Treatment,d)
    if (input$replicates>1) {res <- lm(response~Block+Treatment,d)}
    anova(res)
  })
  
  # Anova sin el factor del bloque
  output$ANOVA_CRD <- renderPrint({
    d <- dataBlocksSim() 
    d <- d$data %>% as.data.frame()
    d$Treatment <- factor(d$Treatment)
    d$Block <- factor(d$Block)
    
    res <- lm(response~Treatment,d)
    anova(res)
  })
  
  
  ## Plot pairwise comparison RCBD
  
  output$ScaleTukeyCI <- renderUI({
    val <- input$MaxScaleTukey
    sliderInput('ScaleTukey','Scale for CI',value=c(-val,val),min=-val,max=val,width="100%")
    
  })
    
  
  output$plot_emmeans_RCBD <- renderPlot({
    
    datos <- dataBlocksSim() 
    datos <- datos$data %>% as.data.frame()
    
    datos$Treatment <- factor(datos$Treatment)
    datos$Block <- factor(datos$Block)
    
    
    res <- lm(response~Treatment+Block,datos)
    
    if (input$replicates>1) {res <- lm(response~Treatment*Block,datos)}
    tky1 = as.data.frame(TukeyHSD(aov(res))$Treatment)
    tky1$pair = rownames(tky1)
    #ymin <- (yTukey)[1]
    #ymax <- (yTukey)[2]
    ymin <- input$ScaleTukey[1]
    ymax <- input$ScaleTukey[2]
    
    # Plot pairwise TukeyHSD comparisons and color by significance level
    p10 <-  ggplot(tky1, aes(colour=cut(`p adj`, c(0, 0.01, 0.05, 1), 
                                        label=c("p<0.01","p<0.05","Non-Sig")))) +
      geom_hline(yintercept=0, linetype ='dashed') +
      geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.2,size=0.8) +
      geom_point(aes(pair, diff),size=2) +
      labs(colour="") +
      ggtitle(('Model = response ~ Block + Treatment')) +
      theme(axis.text = element_text(size = 14),
            axis.title=element_text(size = 16),
            plot.title = element_text(hjust = 0.5, size = rel(1.5))) +
      labs(y="CI for the effect's difference", x="Treatments compared")+
      ylim(ymin,ymax)
    
    p10
    
  })
  
  ## Plot pairwise comparison CRD
  
  output$plot_emmeans_CRD <- renderPlot({
    
    datos <- dataBlocksSim()
    datos <- datos$data %>% as.data.frame()
    
    datos$Treatment <- factor(datos$Treatment)
    datos$Block <- factor(datos$Block)
    
    res <- lm(response~Treatment,datos)
    tky1 = as.data.frame(TukeyHSD(aov(res))$Treatment)
    tky1$pair = rownames(tky1) 
    ymin <- input$ScaleTukey[1]
    ymax <- input$ScaleTukey[2]
    
    # Plot pairwise TukeyHSD comparisons and color by significance level
    p10 <-  ggplot(tky1, aes(colour=cut(`p adj`, c(0, 0.01, 0.05, 1), 
                                        label=c("p<0.01","p<0.05","Non-Sig")))) +
      geom_hline(yintercept=0, linetype ='dashed') +
      geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.2,size=0.8) +
      geom_point(aes(pair, diff),size=2) +
      labs(colour="") +
      ggtitle(('Model = response ~ treatment')) +
      theme(axis.text = element_text(size = 14),
            axis.title=element_text(size = 16),
            plot.title = element_text(hjust = 0.5, size = rel(1.5))) +
      labs(y="CI for the effect's difference", x="Treatments compared")+
      ylim(ymin,ymax)
    
    p10
    
  })
  

  ## Curva de potencia para el diseño en bloques 
  
  output$NewCurvePower <- renderPlotly({

    x1 <- 2
    x2 <- 30
    nrep <- seq(x1,x2,1)
    
    b <- input$cols
    t <- input$rows
    r <- input$replicates
    
    MM=mean(input$mu+t(dT1()))
    mm=input$mu+t(dT1())
    delta2=sum((MM-mm)^2)
    
     
    lambda  <- (r * b * delta2)/((input$sigma)^2)
    
    df1 <- t - 1
    df2 <- t*b*r-t-b+1
    
    Ua <- qf(1-input$alfa, df1 = df1, df2 = df2)
    
    powerC <- 1 - pf(Ua, df1=df1, df2=df2, ncp=lambda)
     
    
    
    lambdaPlot <- c(b*r*delta2/((input$sigma)^2))
    UaP <- qf(1-input$alfa, df1 = df1, df2 = df2)
    power <- 1- pf(UaP, df1, df2=df2, ncp=lambdaPlot[1])
    
    
    for (i in 2:20) {lambdaPlot <- c(lambdaPlot,i * b* delta2/((input$sigma)^2))}
    for (i in 2:length(lambdaPlot)) {UaP <- qf(1-input$alfa, df1 = df1, df2 = (i*t*b-1)-(b-1)-(t-1))
                                    power <- c(power,1- pf(UaP, df1, df2=(i*t*b-1)-(b-1)-(t-1), ncp=lambdaPlot[i]))}
    
    
   
    
    
    data <- data.frame(blocks=1:20,power=power,lambda=lambdaPlot) 
      
    
    plot <- ggplot(data, aes(x = blocks, y = power))+
      geom_line(n=300)+
      theme(axis.text=element_text(size=10), 
            axis.title=element_text(size=10), 
            legend.text=element_text(size=14),
            plot.title = element_text(hjust = 0.5))+
      geom_segment(aes(x = r, y = 0, xend = r, yend = powerC),
                   linetype = "dashed", colour='orange')+
      geom_segment(aes(x = 0, y = powerC, xend = r, yend = powerC),
                   linetype = "dashed", colour='orange')+
      geom_point(aes(x=r,y=powerC), colour="blue", size=2)+
      annotate(geom = "text",x=5, y=0.3, label = paste("The power is: ",round(powerC,4)))+
      labs(x='Replicates', y='Power')
    
    
    ggplotly(plot)
    
    
  }) 
  
  output$EmpiricalPower_RCBD <- renderPlot({
    
    p <- plot_sim_power()$plot
    power <- plot_sim_power()$power
    powert <- plot_sim_power()$powert
    Ua <- plot_sim_power()$Ua
    Ua.CRD <- plot_sim_power()$Ua.CRD
    
    pp <-p + xlim(0,input$ScaleFSim)+
      ylim(0,input$ScalePowerSim)+
      xlab("F values observed in the simulations")+
      ylab("Density")+
      #   geom_vline(xintercept = ua,color="red",linetype="dashed")+
      
      annotate(geom = "text",x=input$ScaleFSim/2,
               size=8,y=input$ScalePowerSim-input$ScalePowerSim/10, label = paste("Simulated power: ",round(power,4)))+
      annotate(geom = "text",x=input$ScaleFSim/2,
               size=8,y=input$ScalePowerSim, label = paste("Theoretical power: ",powert))+
      annotate(geom = "text",x=input$ScaleFSim/2,
               size=8,y=input$ScalePowerSim-input$ScalePowerSim/5, label = paste("Critical F value: ",round(Ua,2)))+
      geom_point(aes(x=Ua,y=0),size=3,shape=5)+
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 22))
    
    p.CRD <- plot_sim_power()$plot.CRD 
    power <- plot_sim_power()$power.CRD 
    
    powert <- plot_sim_power()$powert.CRD 
    
    pp.CRD <- p.CRD + xlim(0,input$ScaleFSim)+
      ylim(0,input$ScalePowerSim)+
      xlab("F values observed in the simulations")+
      ylab("Density")+
        # geom_vline(xintercept = ua,color="red",linetype="dashed")+

      annotate(geom = "text",x=input$ScaleFSim/2,
               size=8,y=input$ScalePowerSim-input$ScalePowerSim/10, label = paste("Simulated power: ",round(power,4)))+
      annotate(geom = "text",x=input$ScaleFSim/2,
               size=8,y=input$ScalePowerSim, label = paste("Theoretical power: ",powert ))+
      annotate(geom = "text",x=input$ScaleFSim/2,
               size=8,y=input$ScalePowerSim-input$ScalePowerSim/5, label = paste("Critical F value: ",round(Ua.CRD,2)))+
      geom_point(aes(x=Ua.CRD,y=0),size=3,shape=5)+
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 22))
    
    ggarrange(pp,pp.CRD)
    
  })
  
  
  ComputeLambda <-  function(b,r,s,tau) {
    d <- sum((tau-mean(tau))^2)
    d/s^2*b*r
  }
  
  ComputeLambdaCRD <- function(b,r,s) {
    eblock <- dataBlocksSim()
    tau <- t(dT1())
    t <- length(tau)
    varb <- var(eblock$eBlock)
    s2 <- s^2+varb
    rho <- varb/s2
    m <- t*r
    DE <- 1+(r-1)*rho
    Neff <- b*r*t/DE
    
    
    d2 <- sum((tau-mean(tau))^2)/t
    
    # Neff*d2/s2
    
    b*r*t*d2/s2
  }
  
  
  TablePower <- function(t,s,tau,alfa=0.05) {
    
    res <- data.frame()
    
    for (b in 2:15) {
      for (r in 1:30) {
        
        df1 <- t-1
        df2 <- t*b*r-t-b+1
        Ua <- qf(1-alfa,df1,df2)
        lambda <- ComputeLambda(b,r,s,tau)
        res <- rbind(res,data.frame(treatments=t,blocks=b,replicates=r,n=b*r*t,power=1-pf(Ua,df1,df2,lambda)))
      }}
    res
  }
  
  output$ComputeActualPower <- renderUI({
    s <- input$sigma
    t <- input$rows
    b <- input$cols
    r <- input$replicates
    alfa <- input$alfa
    tau <- t(dT1())
    lambda <- ComputeLambda(b,r,s,tau)
    df1 <- t-1
    df2 <- t*b*r-t-b+1
    Ua <- qf(1-alfa,df1,df2)
    power <- round(1-pf(Ua,df1,df2,lambda),4)
    
    text <- paste("The statistical power of the actual design, with ",t," treatments, ",
                  b, " blocks, and ",r," replicates is ", power)
    text
  })
  
  # output$ComputeActualPowerNoBlockEffect <- renderUI({
  #   s <- input$sigma
  #   t <- input$rows
  #   b <- input$cols
  #   r <- input$replicates
  #   N <- t*b*r
  #   tau <- t(dT1())
  #   d <- sum((tau-mean(tau))^2)
  #   
  #   lambda <- d/s^2*b
  #   df1 <- t-1
  #   df2 <- N-t
  #   Ua <- qf(0.95,df1,df2)
  #   power <- round(1-pf(Ua,df1,df2,lambda),4)
  #   
  #   text <- paste("The statistical power if blocks effects are neglected, is ", power)
  #   text
  # })
  
  output$ComputeForPower <- renderTable({
    
    goal <- input$GoalPower
    s <- input$sigma
    t <- input$rows
    alfa <- input$alfa
    tau <- t(dT1())
    res <- TablePower(t=t,s=s,tau=tau,alfa=alfa)
    goalLow <- goal - input$MarginGoal
    goalUp <- goal + input$MarginGoal
    resfin <- res %>% filter(power > goalLow & power < goalUp) %>% select(-treatments)
    return(resfin)
  })
  
  ## Función para obtener los cocientes F
  
  cocienteF <- function(trat_sim,blocks_sim,repli_sim,error_sim,mean){
    
    # nulo <- runif(blocks_sim, 0, 1)
    # bajo <- runif(blocks_sim, 1, 10)
    # medio <- runif(blocks_sim, 20, 50)
    # alto <- runif(blocks_sim, 60, 120)
    
    eBlock <- dataBlocksSim()
    eBlock <- t(eBlock$eBlock)
    eTreatment <- t(dT1())
    
    # if(input$opcion == "Nulo") {
    #   eF1 <- t(dF1())
    #   #eF2 <- t(dF2())
    #   eF2 <- t(nulo)
    # }
    # else if (input$opcion == "Bajo") {
    #   eF1 <- t(dF1())
    #   #eF2 <- t(dF2())
    #   eF2 <- t(bajo)
    # }
    # else if (input$opcion == "Medio") {
    #   eF1 <- t(dF1())
    #   #eF2 <- t(dF2())
    #   eF2 <- t(medio)
    # }
    # else {
    #   eF1 <- t(dF1())
    #   #eF2 <- t(dF2())
    #   eF2 <- t(alto)
    # }
    # 

    data <- data.frame(
      Treatment=rep(rep(1:trat_sim,1),blocks_sim),
      Block=rep(rep(1:blocks_sim),trat_sim) %>% sort()
    )
    ra1 <- trat_sim
    ca1 <- blocks_sim
    n1 <- repli_sim
    k1 <- ra1*ca1*n1
    
    ss1 <- error_sim
    
    data$Treatment <- as.factor(data$Treatment)
    data$Block <- as.factor(data$Block)
    
    data <- data %>% mutate(response=mean+eTreatment[Treatment]+eBlock[Block]+rnorm(k1,0,ss1))
    
    mod0 <- anova(lm(response ~ Treatment + Block,data))
    
    f1 <- mod0$`F value`[1]
    
    return(f1)
    
  }
  
  ## Función para crear el gráfico de potencia teórica y por simulación
  
  simulationPower <- function(num_sim,mu_sim,sigma_sim,rows_sim,cols_sim,repli_sim,alfa=0.05){
    
    cssj <- block_cssj()
    
    pnc <- cols_sim*repli_sim*cssj/sigma_sim^2
    
    
    f <- replicate(num_sim, cocienteF(rows_sim,cols_sim,sigma_sim,mu_sim)) # cocientes F simulados
    data <- data.frame(f) # cocientes F en forma de tabla
    
    lambda <- pnc # parametro de no centralidad
    
    # Distribución F de simulación y la F teorica
    ua <- qf(p = 1-alfa, df1 = rows_sim-1, 
             df2 = (cols_sim - 1)*(rows_sim - 1))
    
    power <- sum(f>ua)/num_sim
    
    powert <- 1-pf(ua,rows_sim-1,
                   (cols_sim - 1)*(rows_sim - 1),lambda)%>%round(2)
    
    
    p5 <- ggplot(data, aes(f))+
      geom_histogram(binwidth = 0.3,aes(y = after_stat(density)), color="black", fill="yellow",
                     alpha = 0.5)+
      stat_function(
        fun = df,
        geom = "area",
        linetype = 2,
        fill = "gray",
        alpha = .7,
        args = list(
          df1 = rows_sim-1,
          df2 = (cols_sim - 1)*(rows_sim - 1),
          ncp = lambda)) +
      
      stat_function(
        fun = df,
        geom = "area",
        linetype = 2,
        fill = "red",
        alpha = .6,
        args = list(
          df1 = rows_sim-1,
          df2 = (cols_sim - 1)*(rows_sim - 1),
          ncp = lambda
        ), 
        xlim = c(ua, 30)
      )+
      
      stat_function(
        fun = df,
        geom = "line",
        linetype = 2,
        args = list(
          df1 = rows_sim-1,
          df2 = (cols_sim - 1)*(rows_sim - 1)
        )
      ) + 
      xlim(0,30)+
      annotate(geom = "text",x=10, y=0.2, label = paste("Potencia por simulación: ",power))+
      annotate(geom = "text",x=10, y=0.25, label = paste("Potencia teórica: ",powert))+
      theme(axis.title = element_text(size = 22),
            axis.text = element_text(size = 16))
    
    return(p5)
    
  }
  
  # -------- RCBD power & design optimizer (additive model) --------
  
  # Convert MDE to Cohen's f if needed (equally spaced means assumption).
  # Either supply 'f' directly, or provide one of:
  #   - mde_range: max(min) - min(mean) across k groups (Δ)
  #   - mde_adjacent: difference between consecutive means under equal spacing (δ)
  f_from_mde <- function(k, sigma_within, mde_range = NULL, mde_adjacent = NULL) {
    if (!is.null(mde_range) && !is.null(mde_adjacent)) {
      stop("Provide only one of mde_range or mde_adjacent.")
    }
    if (!is.null(mde_range)) {
      return( (mde_range / sigma_within) * sqrt( (k + 1) / (12 * (k - 1)) ) )
    }
    if (!is.null(mde_adjacent)) {
      return( (mde_adjacent / sigma_within) * sqrt( ((k + 1) * (k - 1)) / 12 ) )
    }
    stop("Either 'f' must be supplied to optimizer() or one of mde_range/mde_adjacent with sigma_within here.")
  }
  
  # Power for treatment F-test in RCBD (additive, no interaction)
  rcbd_power <- function(t, b, r, f, alpha = 0.05) {
    df1 <- t - 1
    df2 <- t * b * r - t - b + 1
    if (df2 <= 0) return(NA_real_)
    lambda <- (b * r) * t * f^2
    Fcrit <- qf(1 - alpha, df1, df2)
    power <- 1 - pf(Fcrit, df1, df2, ncp = lambda)
    return(power)
  }
  
  # Optimizer over integer b and r, returning minimal N that achieves target power.
  optimize_rcbd <- function(t,
                            alpha = 0.05,
                            power_target = 0.80,
                            # supply either f, or (mde_range or mde_adjacent) + sigma_within
                            f = NULL,
                            sigma_within = NULL,
                            mde_range = NULL,
                            mde_adjacent = NULL,
                            b_grid = 2:20,
                            r_grid = 1:10,
                            prefer = c("min_N", "more_blocks")) {
    
    prefer <- match.arg(prefer)
    if (is.null(f)) {
      if (is.null(sigma_within)) stop("Provide sigma_within when deriving f from MDE.")
      f <- f_from_mde(k = t, sigma_within = sigma_within,
                      mde_range = mde_range, mde_adjacent = mde_adjacent)
    }
    
    results <- expand.grid(b = b_grid, r = r_grid)
    results$N_total <- t * results$b * results$r
    results$power <- mapply(function(b, r) rcbd_power(t, b, r, f, alpha), results$b, results$r)
    
    # Keep feasible (non-NA) and meeting power
    ok <- is.finite(results$power) & results$power >= power_target
    cand <- results[ok, , drop = FALSE]
    if (nrow(cand) == 0) return(list(best = NULL, table = results[order(results$N_total, results$power, decreasing = c(FALSE, TRUE)), ]))
    
    # Choose best per preference
    cand <- cand[order(cand$N_total, -cand$power, cand$b, cand$r), ]
    if (prefer == "more_blocks") {
      # Among same N, favor larger b (better control of day-to-day variation)
      cand <- cand[order(cand$N_total, -cand$b, cand$r, -cand$power), ]
    }
    
    best <- cand[1, ]
    list(best = best, table = cand)
  }
  
  output$OptimalDesign <- renderTable({
    tau <- t(dT1())
    t <- length(tau)
    s <- sum((tau-mean(tau))^2)/t
    sigma <- input$sigma
    f <- sqrt(s)/sigma
    
    sol <- optimize_rcbd(t = t, f = f, alpha = 0.05, power_target = input$target_power,
                         b_grid = 2:20, r_grid = 1:24, prefer = "min_N")
    tab <- sol$table
    colnames(tab) <- c("Blocks","Replicates","N_total","Power")
    tab[1:input$MaxDesigns,]
  })
  
  ## Table Tukey
  
  output$SampleSizeTukey <- output$TukeyCIValues <- renderTable({
    d <- dataBlocksSim() 
    d <- d$data %>% as.data.frame()
    d$Treatment <- factor(d$Treatment)
    d$Block <- factor(d$Block) 
    res <-aov(response ~ Treatment + Block,d)
    anova_table <- summary(res)
    MSE <- anova_table[[1]]$`Mean Sq`[3]  # Adjust if structure of summary output changes    
    
    conf <- input$Conf.CI.Tukey
     
    
    t <- input$rows
    b <- input$cols
    r <- input$replicates
    
    
     
    q_critical <- qtukey(conf, t, df =200)
    se_diff <- sqrt(  MSE / (r * b))
    
    treatment_means <- d %>%
      group_by(Treatment) %>%
      summarise(m = mean(response))
    
    ci_table <- data.frame(Comparison = character(), 
                           Diff=numeric(),
                           Precission=numeric(),
                           Lower_CI = numeric(), 
                           Upper_CI = numeric(), stringsAsFactors = FALSE)
    
    # Loop through each pair of treatments
    for (i in 1:(t - 1)) {
      for (j in (i + 1):t) {
        mean_diff <- (treatment_means$m[i] - treatment_means$m[j])
        lower_ci <- mean_diff - q_critical * se_diff
        upper_ci <- mean_diff + q_critical * se_diff
        
        # Store results in the table
        ci_table <- rbind(ci_table, data.frame(
          Comparison = paste(treatment_means$Treatment[i], "vs", treatment_means$Treatment[j]),
          Diff=mean_diff,
          Precission = mean_diff-lower_ci,
          Lower_CI = lower_ci,
          Upper_CI = upper_ci
        ))
      }
    }
    
    ci_table
})
  

  
  ## Required smple size for a given precission RCBD
  
  output$DesiredPrecissionTukey <- renderUI({
    
    t <- input$rows
    b <- input$cols
    r <- input$replicates
    sigma <- input$sigma
    conf <- input$Conf.CI.Tukey
    
    
    qtukey_critical <- qtukey(conf, t, df = 200)  # Large df approximation
    
    # Function to calculate required number of replicates for desired CI precision
    find_replicates_for_precision <- function(treatments, blocks, alpha, desired_precision, sigma) {
      # Rearrange Tukey's HSD formula to solve for replicates (r)
      # desired_precision = qtukey_critical * sqrt(MSE / (r * blocks))
      
      required_replicates <- ceiling((qtukey_critical * sigma / desired_precision)^2 / blocks)
      return(required_replicates)
    }
    
    find_blocks_for_precision <- function(treatments, replicates, alpha, desired_precision, sigma) {
      # Rearrange Tukey's HSD formula to solve for blocks (b)
      # desired_precision = qtukey_critical * sqrt(MSE / (replicates * blocks))
      
      required_blocks <- ceiling((qtukey_critical * sigma / desired_precision)^2 / replicates)
      return(required_blocks)
    }
    
    delta <- input$DesiredPrecission
    size <- switch (input$OptionPrecission,
                    'IncBlocks'=    find_blocks_for_precision(t,r,input$alfa,delta,sigma),
                    'IncReplicates'= find_replicates_for_precision(t,b,input$alfa,delta,sigma)
     )
    

    
    text <- switch (input$OptionPrecission,
                    'IncBlocks'= paste("The required number of blocks are ",size),
                    'IncReplicates'=paste("The required number of replicates are ",size)
    )
  })
  



#########################################################################################333
#################### Data generation for simulation of empirical power****************************************


#### This function generates a simulation for the RCBD

  data_Blocks_sim <- function(rows,
                              cols,
                              repli,
                              sigma){
    ### Treatment effects

    eF1 <- t(dT1())

    ###### We take the effect blocks generated in the scenario
    
    
    eBlock <- dataBlocksSim()
    eF2 <- t(eBlock$eBlock)
    
    ############ Generate RCBD structure

    data <- data.frame(
      F1=rep(rep(1:rows,1),cols*repli),
      F2=rep(rep(1:cols),rows) %>% sort()
    )
    
    ###### Data is generated as a repetition of the basic scenario

    data <- data %>% mutate(response=input$mu+eF1[F1]+eF2[F2]+rnorm(rows*cols*repli,0,sigma))

    data$F1 <- as.factor(data$F1)
    data$F2 <- as.factor(data$F2)

    res <- anova(lm(response ~ F2+F1,data))
    r3 <- res$`F value`[1]
    r4 <- res$`F value`[2]
    
    res.CRD <- anova(lm(response ~ F1,data))
    r3.CRD <- res.CRD$`F value`[1]
    

    list(f_blocks=r3,F_Treatments=r4,F_Treatments_CRD=r3.CRD,data=data)
  }


simulation_Designs <- function(num_sim,
                                    mu_sim,
                                    sigma_sim,
                                    repli,
                                    rows,
                                    cols,
                                    eF2,
                                    bin=0.5,
                                    alfa=0.05){

  ##### mean by treatment RCBD

  m <- t(dT1())
  delta <- sum((m-mean(m))^2)
  lambda <- repli*cols*delta/(sigma_sim)^2
  
  f <- c()
  f.CRD <- c()
  
  for (i in 1:num_sim) {
    simu <- data_Blocks_sim(rows=rows,
                            cols=cols,
                            repli=repli,
                            sigma=sigma_sim)
    f <- c(f,simu$F_Treatments)
    f.CRD <- c(f.CRD,simu$F_Treatments_CRD)
  }
    
                              
 ####### RCBD ##########################

  # cocientes F simulados

  data <- data.frame(f) # cocientes F en forma de tabla

  # Distribución F de simulación y la F teorica
  
  ua <- qf(p = 1-alfa, 
           df1 = rows-1,
           df2 = (repli*cols*rows-1)-(cols-1)-(rows-1))

  power <- sum(f>ua)/num_sim %>% round(4)

  powert <- 1-pf(ua,
                 rows-1,
                 (repli*cols*rows-1)-(rows-1)-(cols-1),
                 ncp=lambda)

  powert <- powert %>% round(4)


  df2=(cols*rows*repli-1)-(cols-1)-(rows-1)

  plot <- ggplot(data, aes(sort(f)))+
    geom_histogram(aes(y = ..density..),
                   color="black",
                   fill="yellow",
                   binwidth = bin,
                   boundary = 0,
                   alpha = 0.5)+
    #geom_density(size=1)+
    stat_function(
      fun = df,
      geom = "area",
      linetype = 2,
      fill = "gray",
      alpha = .7,
      args = list(
        df1 = rows-1,
        df2 = df2,
        ncp = lambda)) +
    stat_function(
      fun = df,
      geom = "line",
      color='red',
      n=300,
      size=1,
      args = list(
        df1 = rows-1,
        df2 = df2,
        ncp = lambda)) +
    stat_function(
      fun = df,
      geom = "area",
      linetype = 2,
      fill = "red",
      alpha = .6,
      args = list(
        df1 = rows-1,
        df2 = df2,
        ncp = lambda
      ),
      xlim = c(ua, input$ScaleFSim)
    )+

    stat_function(
      fun = df,
      geom = "line",
      linetype = 2,
      n=300,
      args = list(
        df1 = rows-1,
        df2 = df2
      )
    )
  
  ############## CRD
  
  # cocientes F simulados
  
  data.CRD  <- data.frame(f.CRD) # cocientes F en forma de tabla
  
  # parametro de no centralidad 
  
  # Distribución F de simulación y la F teorica
  
  eblock <- dataBlocksSim()
  tau <- t(dT1())
  t <- length(tau)
  b <- cols
  r <- repli
  s <- sigma_sim
  varb <- var(eblock$eBlock)
  s2 <- s^2+varb
  
  df2 <- t*b*r - t
  
  ua.CRD <- qf(p = 1-alfa, 
               df1 = t-1, 
               df2 = df2)
  
  power.CRD <- sum(f.CRD >= ua.CRD )/num_sim %>% round(4)
  
  
  lambda.CRD = ComputeLambdaCRD(cols,repli,sigma_sim)
  
  
  powert.CRD  <- pf(ua.CRD,
                 rows-1,
                 df2,
                 ncp=lambda.CRD,
                 lower.tail = FALSE)
  
  powert.CRD  <- powert.CRD  %>% round(4)
  
  
  df2=(rows*repli*cols)-rows
  
  plot.CRD  <- ggplot(data.CRD , aes(sort(f.CRD)))+
    geom_histogram(aes(y = ..density..),
                   color="black",
                   fill="yellow",
                   binwidth = bin,
                   boundary = 0,
                   alpha = 0.5)+
    #geom_density(size=1)+
    stat_function(
      fun = df,
      geom = "area",
      linetype = 2,
      fill = "gray",
      alpha = .7,
      args = list(
        df1 = rows-1,
        df2 = df2,
        ncp = lambda.CRD)) +
    stat_function(
      fun = df,
      geom = "line",
      color='red',
      n=300,
      size=1,
      args = list(
        df1 = rows-1,
        df2 = df2,
        ncp = lambda.CRD)) +
    stat_function(
      fun = df,
      geom = "area",
      linetype = 2,
      fill = "red",
      alpha = .6,
      args = list(
        df1 = rows-1,
        df2 = df2,
        ncp = lambda.CRD
      ),
      xlim = c(ua, input$ScaleFSim)
    )+
    
    stat_function(
      fun = df,
      geom = "line",
      linetype = 2,
      n=300,
      args = list(
        df1 = rows-1,
        df2 = df2
      )
    )
  

  list(plot=plot,power=power,powert=powert,plot.CRD=plot.CRD,power.CRD=power.CRD,powert.CRD=powert.CRD,Ua=ua,Ua.CRD=ua.CRD)

}

# Generate data and plot of power from simulations

plot_sim_power <- eventReactive(input$go,{#if (input$fix==TRUE){set.seed(input$valfix)}
  #else set.seed(round(runif(1,1,10000),0))

  simulation_Designs(num_sim=input$num_sim,
                 mu_sim=input$mu,
                 sigma_sim=input$sigma,
                 repli=input$replicates,
                 rows=input$rows,                  # treatments
                 cols=input$cols,                  # blocks
                 eF2=dataBlocksSim()$eBlock,
                 bin=input$binwSimPower,
                 alfa=input$alfa)

}, ignoreNULL = F)


############ Showing the concept of p-value

output$PValueCase <- renderPlot({
  
  d <- dataBlocksSim() 
  d <- d$data %>% as.data.frame()
  d$Treatment <- factor(d$Treatment)
  d$Block <- factor(d$Block)
  
  res <- lm(response~Block+Treatment,d)
  res <- anova(res)
  U <- res$`F value`[2] %>% round(4)
  

  data <- data.frame(F=seq(0,input$ScalePValue,.05))
  df1 <- input$rows - 1
  
  r <- input$replicates
  k <- input$rows
  b <- input$cols
  
  
  if (r > 1) {df2=(k*b*r-1)-(k-1)-(b-1)}
    else {df2=(k - 1)*(b -1)}
  
  power <- 1-pf(U,df1,df2) %>% round(4)
  
  if (power > input$alfa) {
    text= "There is no enough evidence of \n a treatment effect"
    color="darkgreen"}
  
    else {
      text= "The results are compatible with the \n existence of a treatment effect" 
      color="darkred"}
  
  ggplot(data,aes(F))+
    stat_function(fun=df,geom='line',
                  args=list(df1=df1,df2=df2))+
    stat_function(fun=df,geom='area',
                  fill='red',alpha=0.4,
                  args=list(df1=df1,df2=df2),
                  xlim=c(U,input$ScalePValue))+
    annotate("text",x=5,y=0.6,size=5,
             label="Computing p-value:")+
    annotate("text",x=5,y=0.5,size=5,
             label=as.expression(bquote(P(F[.(paste(df1,",",df2))]>.(U)) == .(power))))+
    annotate("text",x=5,y=0.4,size=5,color=color,
             label=text)+
    xlim(0,input$ScalePValue)+
    ylim(0,1)
})




############################ Theoretical precission

output$PrecisionTheoretical <- renderUI({
  t <- input$rows
  b <- input$cols
  r <- input$replicates
  s <- input$sigma
  conf <- input$Conf.CI.Tukey
  dfr <- (t*b*r-1)-(t-1)-(b-1)
  qt <- qtukey(conf,t,dfr)
  MSE <- s^2
  HSD <- qt*sqrt(MSE/(b*r))
  text <- paste0("With ",b," blocks and ",r," replicates the expected precision
                 of the CI for the difference in treatments effects is ",round(HSD,3),"")
  text
})

########### Sample size using Cohen f (not yet implemented)

# output$SampleSizeCohen <- renderPrint({
#   eT <- t(dT1())
#   m <- mean(eT)
#   SSB <- sum((eT-m)^2)
#   sigma <- input$sigma
#   f <- sqrt((SSB/(input$rows)))/(sigma)
#   
#   
#   
#   k <- length(eT)
#   power <- input$powerCohen
#   res <- pwr.anova.test(k=k,
#                       f=f,
#                       sig.level=0.05,
#                       power=power)
#   
#   res
# })

output$PlotPowerByBlocks <- renderPlotly({
  
  res <- data.frame()
  resCRD <- data.frame()
  t <- input$rows
  b <- input$NumBlocks
  
  s <- input$sigma
  alfa <- input$alfa
  despower <- 0.8
  tau <- t(dT1())
  
  #### CRD
  for (r in 2:30) {
    
    df1 <- t-1
    df2 <- t*r*b-t
    Ua <- qf(1-alfa,df1,df2)
    lambda <- ComputeLambdaCRD(b,r,s)
    resCRD <- rbind(resCRD,data.frame( replicates=r,lambda=lambda,Ua=Ua,df1=df1,df2=df2,power=1-pf(Ua,df1,df2,lambda)))
  }
  
  #### RCBD
  for (r in 1:30) {
    
    df1 <- t-1
    df2 <- t*b*r-t-b+1
    Ua <- qf(1-alfa,df1,df2)
    lambda <- ComputeLambda(b,r,s,tau)
    res <- rbind(res,data.frame(treatments=t,blocks=b,replicates=r,n=b*r*t,power=1-pf(Ua,df1,df2,lambda)))
  }
  
  r <- input$replicates
  df1 <- t-1
  df2 <- t*b*r-t-b+1
  lambda <- ComputeLambda(b,r,s,tau)
  Ua <- qf(1-alfa,df1,df2)
  actual_power <- 1-pf(Ua,df1,df2,lambda)
  
  p <- ggplot(res,aes(replicates,power))+
    geom_point(shape=21,fill='yellow',size=2)+
    geom_point(data=resCRD,aes(replicates,power),shape=21,fill='white',size=2)+
    geom_line(color='red')+
    geom_line(data=resCRD,aes(replicates,power),color='blue')+
    geom_hline(yintercept = despower,lty=2,color='red')+
    geom_vline(xintercept = input$replicates,lty=2,color='red')+
    geom_point(aes(x=r,y=actual_power),size=4)+
    scale_y_continuous(breaks=seq(0,1,.1))+
    ggtitle(paste("Power vs. replicates with ",b, "blocks"))+
    theme(
      plot.title = element_text(size = 12,
      margin = margin(b = 30))
    )
  
  ggplotly(p)
})

ComputeCohenFromTau <- function(tau=c(0,1,2),sigma=7) {
  n <- 1000
  t <- length(tau)
  SSB <- n*sum((tau-mean(tau))^2)
  SSW <- (n*t-t)*sigma^2
  
  f <- sqrt(SSB/SSW)
  round(f,3)
}

output$Cohenf <- renderUI({
  f <- ComputeCohenFromTau(tau=t(dT1()),sigma=input$sigma)
  # Initialize text variable
  text <- ""
  
  # Use conditional statements
  if (f < 0.2) {
    text <- "low effect. Hence your design require large sample size."
  } else if (f < 0.4) {
    text <- "moderate effect. Hence your design may work with a moderate sample size."
  } else {
    text <- "large effect. Thus you will need a relatively low sample size."
  }
  
  text2 <- "However, the final number of blocks and replicates will depend on the precission you require when estimating the CI for the difference of mean responses among treatments."
  
  text <- withMathJax(paste("With the specified effects, Cohen's \\(f\\)=",f,", which represents a ",text, text2))
  text
    
})




  ####################### END OF SERVER

})

 




