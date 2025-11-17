#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
# Date 04/07/2023
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(rhandsontable)
library(tidyverse)
library(plotly)
library(collapsibleTree)
library(webshot)
library(future)


webshot::install_phantomjs(force=TRUE)

shinyUI(
  #####################
  dashboardPage(skin = "green",
                dashboardHeader(title = HTML("The Randomised Complete Block Design (RCBD) versus Completely Randomized Design (CRD)"),
                  titleWidth = 1000),
                dashboardSidebar(sidebarMenu(
                  menuItem("Home", tabName = "Home", icon = icon("home")),
                  menuItem("Learning goals", icon = icon("star"), tabName = "Goals"),
                  menuItem("What's a RCBD?", icon = icon("circle-question"), tabName = "WhatsRCBD"),
                  menuItem("What's a CRD?", icon = icon("circle-question"), tabName = "WhatsCRD"),
                  #menuItem("Linear model", icon = icon("eye"), tabName = "LinearModel"),
                  menuItem("Simulate experiments", icon = icon("computer"), tabName = "Simulate"),
                  menuItem("Suggested work", icon = icon("book"), tabName = "ExploreCases"),
                  menuItem("Contact", icon = icon("book-open-reader"), tabName = "Contact")
                  
                  )),
                dashboardBody(
                  tags$head(
                    tags$style(HTML("
                          html, body, .wrapper, .content-wrapper, .right-side {
                            background: #ffffff !important;
                            height: auto !important;
                            min-height: 100% !important;
                          }
                          .content {
                            background: #ffffff !important;
                            padding: 20px;
                          }
                          .box {
                            background: #ffffff !important;
                            border: 1px solid #e5e5e5;
                            box-shadow: none;
                            margin-bottom: 20px;
                          }
                        "))
                  ),
                  tabItems(
                    
                    ###############################################################################
                    tabItem(tabName = "Home",
                                width = 12, 
                                high = 12,
                            
                                column(4,
                                       h2("Randomized Complete Block Design (RCBD)"),
                                       h4("A shiny app for understanding the use,
                                       interpretation, and limitations of 
                                       randomized complete block in experimental design.")
                                       ),
                                column(8,
                                       img(src = "img/caratula.jpg", width=600,heigh=600)
                                       )
                                
                            ),
                    #################################################################################
                    tabItem("Goals",
                            hr(),
                            includeHTML("www/HTML_files/Goals.html")),
                    ########################################################################################
                    tabItem("WhatsCRD",
                            withMathJax(),
                            tabsetPanel(id = "inner_tabs_3", type = "pills",
                              tabPanel("What's a CRD?",
                                       br(),
                                       box(width=12,title=HTML("<span style='color:white; font-weight:bold;'>What's a CRD?"),status="primary",solidHeader=TRUE,
                                           includeHTML("www/HTML_files/What_is_CRD.html")
                                           )
                              ),
                              tabPanel("Example of a CRD plan",
                                       br(),
                                       box(width=12,title=HTML("<span style='color:white; font-weight:bold;'>Example of a CRD plan"),
                                           status="primary",solidHeader=TRUE,
                                           includeHTML("www/HTML_files/Example_CRD_Plan.html")
                                       )
                                       ),

                              tabPanel("Examples of CRD",
                                       br(),
                                       box(width=12,title=HTML("<span style='color:white; font-weight:bold;'>Examples"),
                                           status="primary",solidHeader=TRUE,
                                           includeHTML("www/HTML_files/Examples_CRD.html")
                            ))
                            )
                            ),
                    
                    ################################################################################
                    tabItem("WhatsRCBD",
                            withMathJax(),
                            tabsetPanel(id = "inner_tabs_4", type = "pills",
                                        tabPanel("What's a RCBD?",
                                                 br(),
                                                 box(width=12,title=HTML("<span style='color:white; font-weight:bold;'>What's a RCBD?"),
                                                     status="primary",solidHeader=TRUE,
                                                     includeHTML("www/HTML_files/What_is_RCBD.html")
                                                 )),
                                        tabPanel("Example of a RCBD plan",
                                                 br(),
                                                 box(width=12,title=HTML("<span style='color:white; font-weight:bold;'>Example of a RCBD plan"),
                                                     status="primary",solidHeader=TRUE,
                                                     includeHTML("www/HTML_files/Example_RCBD_Plan.html") )),
                                        tabPanel("Examples of RCBD",
                                                 br(),
                                                 box(width=12,title=HTML("<span style='color:white; font-weight:bold;'>Examples of RCBD"),
                                                     status="primary",solidHeader=TRUE,
                                                     includeHTML("www/HTML_files/Examples_RCBD.html")
                                                     
                                                 )
                                        )
                            )),
                    ###################################################################################
                    
                    ########################################################################################
                    tabItem(tabName = "Simulate",

                            fluidPage(

                              column(3,
                                     box(width = 12,title = HTML("<span style='color:white; font-weight:bold;'>Parameters' settings"),
                                         status = "success", solidHeader = TRUE,
                                         fluidRow(
                                         column(6,numericInput('mu',withMathJax(HTML('Baseline <br>mean (\\(\\mu\\))')),value=40)),
                                         column(6,numericInput('sigma',withMathJax(HTML('sigma within (\\(\\sigma_{within}\\))')),value=3,min=0.01))
                                         ),
                                         fluidRow(
                                         column(6,numericInput('alfa',HTML('Significance level <br>(\\(\\alpha\\))'),value=0.05,min=0.001,max=0.2,step=0.01)),
                                         column(6,numericInput("rows", "Number of Treatments:",
                                                     min = 3, max = 6, value = 3,step=1))
                                         ),
                                         radioButtons("ChoiceInputEffects","How do you want to define the treatment effects?",
                                                      choices=c("From Cohen's f"="Cohen",
                                                                "Manually"="Manual")),
                                         conditionalPanel("input.ChoiceInputEffects=='Cohen'",
                                                          sliderInput("fCohen",HTML("Cohen's \\(f\\) <br>(0.1: Low effect, 1:Large effect)"),
                                                                      value=0.2,min=0,max=1,step=.01),
                                                          h5(withMathJax(HTML("Effects of Treatments <br>(calculated from Cohen's \\(f\\))")))
                                                          ),
                                         conditionalPanel("input.ChoiceInputEffects=='Manual'",
                                                          h5("Indicate the effects for each treatment (Change with respect the baseline)")
                                                          ),
                                         rHandsontableOutput("input_table_T"),
                                         br(),
                                         fluidRow(
                                           column(6,
                                                  numericInput('cols',withMathJax(HTML('Number of <br>Blocks')),value=3,min=2,step=1)
                                                  ),
                                           column(6,selectInput(inputId = "opcion", label = HTML("Select <br>block's effect <br> Variability of bock vs. within"),
                                                      choices = c("No effect" = "Nulo",
                                                                  "Low" = "Bajo",
                                                                  "Medium" = "Medio",
                                                                  "Large" = "Alto"),
                                                     selected="Alto"))
                                         ),
                                         numericInput('replicates','How many replicates per treatment in each block?',
                                                      value=4,min=1,step=1),
                                         column(3),
                                         column(3,
                                         actionButton('go',HTML('Generate new <br> simulated data'),
                                                      icon = icon("thumbs-up"),
                                                      style = "color: white; background-color: blue; border-color: blue;")))

                              ),
                            column(9,
                              tabsetPanel(id = "inner_tabs_main", type = "pills",
                                          tabPanel(HTML("Start here"),
                                                h2("Setting-up an scenario"),
                                                radioButtons(
                                                  "SelExplanation", "Select an option",
                                                  choices = c(
                                                    "Explain the meaning of the parameters" = "ExplainParameters",
                                                    "Why It Is Important to Set Up a Scenario in an RCBD?" = "WhyScenario",
                                                    "The importance of simulations" = "ImportanceSimulations"
                                                    
                                                  ),
                                                  selected = "ExplainParameters"
                                                ),
                                                fluidRow(
                                                  # --- Usar input.SelExplanation == '...' ---
                                                  conditionalPanel(
                                                    condition = "input.SelExplanation == 'WhyScenario'",
                                                    div(
                                                      box(title = HTML("<span style='color:white; font-weight:bold;'>Why It Is Important to Set Up a Scenario in an RCBD?"),
                                                          width = 8, status = "primary", solidHeader = TRUE,
                                                          includeHTML('www/HTML_files/Setting_up_Scenario.html')
                                                          )
                                                    )
                                                  ),
                                                  conditionalPanel(
                                                    condition = "input.SelExplanation == 'ImportanceSimulations'",
                                                    div(
                                                      box(title = HTML("<span style='color:white; font-weight:bold;'>The importance of simulations"),
                                                          width = 8, status = "primary", solidHeader = TRUE,
                                                          includeHTML('www/HTML_files/Why_Simulating_fragment_min.html')
                                                          )
                                                    )
                                                  ),
                                                  conditionalPanel(
                                                    condition = "input.SelExplanation == 'ExplainParameters'",
                                                    div(
                                                      box(title = HTML("<span style='color:white; font-weight:bold;'>Explain the meaning of the parameters"),
                                                          width = 12, status = "primary", solidHeader = TRUE,
                                                          fluidRow(
                                                            column(
                                                              6,
                                                              h4(HTML("<b>Baseline mean (\\(\\mu\\))</b>: Define a value of the mean response in absence of treatment and block effects.")),
                                                              h4(HTML("<b>Common sigma (\\(\\sigma\\))</b>: Standard deviation of the response. Assumes homoscedasticity.")),
                                                              h4(HTML("<b>Significance level (\\(\\alpha\\))</b>: Type I error (usually 0.05).")),
                                                              h4(HTML("<b>Number of treatments</b>: Suggested 3–4 for clarity."))
                                                            ),
                                                            column(
                                                              6,
                                                              h4(HTML("<b>Cohen’s \\(f\\)</b>: ANOVA effect size. The app maps \\(f\\) to per-treatment effects; you can override manually.")),
                                                              h4(HTML("<b>Number of blocks</b>: RCBD distributes treatments within each block; you can add replicates within blocks.")),
                                                              h4(HTML("<b>Choose the block effect</b>: Explore different block-variance levels to see impact on results.")),
                                                              h4(HTML("<b>Replicates per treatment per block</b>: Defaults to none; explore replication’s effect on RCBD."))
                                                            )
                                                          )
                                                      ))
                                                  )
                                                )
                                       ),
                                       #####################################################################################
                                tabPanel(HTML("Explore experiment's<br>estructure "),
                                         br(),
                                         box(title=HTML("<span style='color:white; font-weight:bold;'>Which is the difference between Randomized 
                                                        Complete Block Design (RCBD) and a Completely Randomized Design (CRD)"),
                                         width='80%', status = "primary", solidHeader = TRUE,
                                         h4(HTML("A <b>Completely Randomized Design (CRD)</b> is used when all experimental
                                                 units are assumed to be similar, so treatments can be assigned entirely
                                                 at random without considering any grouping or structure in the units.
                                                 In contrast, a <b>Randomized Complete Block Design (RCBD)</b> is used when the
                                                 experimental units can be divided into groups called blocks, where units within
                                                 the same block are more alike than those in different blocks. <br><br>
                                                 In an RCBD, each block receives all treatments, and randomization
                                                 is carried out within each block. This allows the design to separate
                                                 the variation caused by differences among blocks from the variation due
                                                 to treatments. As a result, RCBD provides a more accurate estimate of
                                                 treatment effects when block differences are substantial, whereas CRD
                                                 may inflate experimental error if such natural grouping or variability is ignored."))
                                         ),
                                         hr(),
                                         fluidRow(
                                           column(3,
                                                  radioButtons('structure','Choose the design:',
                                                      choices=c('With Blocks (RCBD)'='blocks',
                                                                'Without Blocks (CRD)'='noblocks'))),
                                           column(3,
                                                  textInput('TreatName','Label for treatments',value='Treatment')),
                                           column(3,
                                                  textInput('BlockName','Label for blocks',value='Block'))),
                                         conditionalPanel("input.structure=='blocks'",
                                                          br(),
                                                          box(
                                                            width = 7,title = HTML("<span style='color:white; font-weight:bold;'>Experimental plan and results using a RCBD"),
                                                            status = "success", solidHeader = TRUE,
                                                            collapsibleTreeOutput('design_RCBD',width = "100%", height = "500px")),
                                                          box(
                                                            width = 5,title = HTML("<span style='color:white; font-weight:bold;'>Design considering blocks (RCBD)"),
                                                            status = "primary", solidHeader = TRUE,
                                                            h4(HTML("Observe the response to each treatment when the block effect is taken into account.
                                                               It is important to note that the observed response is the combined result of both
                                                               the treatment effect and the block effect. <br><br>
                                                                    Hence, you may observe large differences in the response to a given treatment
                                                                    when the block effect is substantial. Failing to account for the block effect
                                                                    can lead to misinterpreting the variability in the treatment responses."))
                                                          )),
                                         conditionalPanel("input.structure=='noblocks'",
                                                          br(),
                                                          box(
                                                            width = 7,title = HTML("<span style='color:white; font-weight:bold;'>Experimental plan and results using a CRD"),
                                                            status = "success", solidHeader = TRUE,
                                                            collapsibleTreeOutput('design_CRD',width = "100%", height = "500px")),
                                                          box(
                                                            width = 5,title = HTML("<span style='color:white; font-weight:bold;'>Design without considering blocks (CRD)"),
                                                            status = "primary", solidHeader = TRUE,
                                                            h4(HTML("If you do not consider block effects, you inflate the
                                                                    variability of the treatment responses.
                                                                    Failing to account for block effects can lead to poor
                                                                    estimation of treatment effects, especially when the block
                                                                    effects are large."))
                                                          ))
                                ),
                              ######################################################################################                                         
                                tabPanel('Descriptive',
                                         fluidPage(

                                           column(12,
                                                  
                                                  radioButtons("SelDescr","Select descriptive",
                                                               choices=c(
                                                                 "Randomized Controlled Block design (RCBD)"="RCBD",
                                                                 "Completely randomized design (CRD)"="CRD"
                                                               )),
                                                  conditionalPanel("input.SelDescr=='RCBD'",
                                                                   box(
                                                                     width = 12,title = HTML("<span style='color:white; font-weight:bold;'>Results by block and treatment (RCBD)"),
                                                                     status = "primary", solidHeader = TRUE,
                                                                     h4(HTML("The panels show the treatment responses within each block in a Randomized Complete Block Design (RCBD).
                                                                             Each colored dashed line represents one block, and within each block we can see how the response
                                                                             changes across the three treatments. The key point is that the overall pattern across blocks is similar:
                                                                             in every block, the response increases from Treatment 1 to Treatment 3. This suggests that the treatment effect
                                                                             is consistent, while the difference in the vertical position of the lines reflects the block effect (some blocks
                                                                             have higher overall response levels than others). By accounting for these block-to-block shifts, the RCBD helps
                                                                             isolate the real treatment differences and provides a more precise comparison among treatments."))
                                                                   ),
                                                                   box(
                                                                     width = 6,title = HTML("<span style='color:white; font-weight:bold;'>Summary by treatment and block (RCBD)"),
                                                                     status = "success", solidHeader = TRUE,
                                                                     tableOutput('TableMeansRCBD')
                                                                   ),
                                                                   box(
                                                                     width = 6,title = HTML("<span style='color:white; font-weight:bold;'>Descriptive Plot"),
                                                                     status = "success", solidHeader = TRUE,
                                                                     numericInput("MaxScaleyRange","Max value for scale y",value = 40),
                                                                     uiOutput("ScaleyRange"),
                                                                     
                                                                     plotOutput('PlotMeansRCBD')
                                                                   )),
                                                  conditionalPanel("input.SelDescr=='CRD'",
                                                                   box(
                                                                     width = 12,title = HTML("<span style='color:white; font-weight:bold;'>Results by treatment (CRD)"),
                                                                     status = "primary", solidHeader = TRUE,
                                                                     h4(HTML("The panels show the estimated mean response for each treatment in a Completely Randomized
                                                                     Design (CRD), where no blocking factor is included. Each point represents the average response for a
                                                                     treatment, and the vertical error bars indicate the variability (standard error or confidence interval)
                                                                     around those means.<br><br>
                                                                             Because blocks are not considered in a CRD, all variation among experimental units
                                                                             is treated as random error. This means that any real differences among groups of experimental units
                                                                             (such as environmental or time-related differences) are not separated out, and instead contribute
                                                                             to the residual variability. As a result, the treatment means are still comparable, but the precision
                                                                             of those estimates may be lower than in a blocked design—reflected by the wider spread in the error bars.
                                                                             In short, the design still allows us to compare treatments, but without blocking we cannot account for
                                                                             potential nuisance variation that could have been controlled."))
                                                                   ),
                                                                   box(
                                                                     width = 6,title = HTML("<span style='color:white; font-weight:bold;'>Summary by treatment (CRD)"),
                                                                     status = "success", solidHeader = TRUE,
                                                                     tableOutput('TableMeansCRD')
                                                                   ),
                                                                   box(
                                                                     width = 6,title = HTML("<span style='color:white; font-weight:bold;'>Descriptive Plot"),
                                                                     status = "success", solidHeader = TRUE,
                                                                     sliderInput('yRange','Range for response axes',value=c(0,40),min=-100,max=300),
                                                                     plotOutput('PlotMeansCRD')
                                                                   ))
                                           )

                                         )),
                              ##############################################################################
                              tabPanel(HTML('Design matters!! <br>RCBD vs CRD'),
                                       h3("Explore RCBD and CRD results"),
                                       tabsetPanel(id = "inner_tabs", type = "pills",
                                                   tabPanel(HTML("RCBD vs. CRD"),
                                                            fluidPage(
                                                              br(),
                                                              box(width = 12,title = HTML("<span style='color:white; font-weight:bold;'>Analysis of variance"),
                                                                  status = "primary", solidHeader = TRUE,
                                                                  h4(HTML("In this part, the ANOVA table is shown as well as the multiple
                                  comparisons graph by Tukey's method.
                                  Analysis is done with and without considering the block effect.
                                          If the block effect is important, the estimation of effects
                                          without including that effect produces wider confidence intervals.")),
                                                                  h4(uiOutput('Cohenf'))),
                                                              numericInput("MaxScaleTukey","Maximum value for the scale of CI",value=100),
                                                              # sliderInput('ScaleTukey','Scale for CI',value=c(-20,20),min=-1000,max=1000,width="100%"),
                                                              uiOutput("ScaleTukeyCI"),
                                                              fluidPage(
                                                                column(6,
                                                                       box(width = 12,title = HTML("<span style='color:white; font-weight:bold;'>Analysis of variance with block effects"),
                                                                           status = "success", solidHeader = TRUE,
                                                                           verbatimTextOutput('ANOVA_RCBD'))
                                                                ),
                                                                column(6,
                                                                       box(width = 12,title = HTML("<span style='color:white; font-weight:bold;'>Analysis of variance without block effects"),
                                                                           status = "success", solidHeader = TRUE,
                                                                           verbatimTextOutput('ANOVA_CRD')))
                                                              ),
                                                              fluidPage(
                                                                column(6,
                                                                       box(width = 12,title = HTML("<span style='color:white; font-weight:bold;'>Treatment effects with block effects"),
                                                                           status = "success", solidHeader = TRUE,
                                                                           plotOutput('plot_emmeans_RCBD')))
                                                                ,
                                                                column(6,
                                                                       box(width = 12,title = HTML("<span style='color:white; font-weight:bold;'>Treatment effects without block effects"),
                                                                           status = "success", solidHeader = TRUE,
                                                                           plotOutput('plot_emmeans_CRD')))
                                                              )
                                                              
                                                            )),
                                                   ###################################################################################################################
                                                   tabPanel(HTML("Explanation of <br> ANOVA table"),
                                                            fluidPage(
                                                              h3("Explantion of the terms in the ANOVA table"),
                                                              hr(),
                                                              box(width = 12,title = HTML("<span style='color:white; font-weight:bold;'>ANOVA table"),
                                                                  status = "primary", solidHeader = TRUE,
                                                                  img(src = "ANOVATableFormulas.PNG", width='100%')
                                                              ),
                                                              column(6,
                                                                     
                                                                     box(width = 12,title = HTML("<span style='color:white; font-weight:bold;'>Analysis of variance with block effects"),
                                                                         status = "success", solidHeader = TRUE,
                                                                         verbatimTextOutput('ANOVA_RCBD_bis')),
                                                                     box(width = 12,title = HTML("<span style='color:white; font-weight:bold;'>How to compute the p-value"),
                                                                         status = "success", solidHeader = TRUE,
                                                                         h4(HTML("The p-value is computed as the probability of
                                                                                  a F-ratio result (MsTreat/MsResidual) greater that the
                                                                                  value obtained in the ANOVA table. Under the null
                                                                                       hypothesis, the expected distribution is a F distribution
                                                                                       with (t-1) and (t*b*r-1)-(t-1)-(b-1) degrees of freedom.")),
                                                                         hr(),
                                                                         numericInput('ScalePValue','Scale for F values',value=10),
                                                                         plotOutput('PValueCase')
                                                                     )),
                                                              column(6,
                                                                     box(width = 12,title = HTML("<span style='color:white; font-weight:bold;'>Formulas"),
                                                                         status = "primary", solidHeader = TRUE,
                                                                         h5(HTML("Note that there is no F ratio for the block, since the block
                                                          is included to homogenize the experimental units..<br><br>

                                                          The elements of the sum of squares for ANOVA table, are given by:

                                                          $$ssTreat=\\frac{b \\times r}{t}\\displaystyle\\sum_{j=1}^{t}(\\bar y_{j}-\\bar y)^{2}$$


                                                          $$ssBlock=\\frac{t \\times r}{b}\\displaystyle\\sum_{i=1}^{b}(\\bar y_{i}-\\bar y)^{2}$$


                                                          $$ssTotal = \\displaystyle\\sum_{i=1}^b\\displaystyle\\sum_{j=1}^t
                                                          \\displaystyle\\sum_{k=1}^r(y_{ijk}-\\bar y)^2$$

                                                          $$ssRes=ssTotal-ssBlock-ssTreat$$

                                                          The corresponding degrees of freedom and mean squares are indicated in the table.

                                                          "))
                                                                         
                                                                     ),
                                                                     box(width = 12,title = HTML("<span style='color:white; font-weight:bold;'>Computing p-value"),
                                                                         status = "primary", solidHeader = TRUE,
                                                                         h4(HTML("The p-value (Pr(>F) in the R ANOVA table) corresponds to the probability:
                                                   $$P\\left(F_{dfTreat, dfRes}>\\frac{msTreat}{msResidual}\\right)$$")))
                                                                     
                                                              )
                                                            ))
                                       )),
                              #############################################################################################
                              tabPanel(HTML('Statistical <br>power'),
                                       h4("Explore statistical power"),
                                       tabsetPanel(id = "inner_tabs_2", type = "pills",
                                                   tabPanel("What does it mean?",
                                                            
                                                            box(width = 8,title = HTML("<span style='color:white; font-weight:bold;'>Statistical power"),
                                                                status = "primary", solidHeader = TRUE,
                                                                h4(HTML("What proportion of experiments will detect the presence of a treatment effect?
                                                                       The effects you specified represent the minimum effect sizes that you want the design to reliably detect
                                                                       (i.e., the effect size used to calculate statistical power).
                                                                       Try adjusting the <b>number of blocks</b> to see how blocking influences the power of the experiment
                                                                       to detect those treatment differences."))
                                                            ),
                                                            box(width = 12,title = HTML("<span style='color:white; font-weight:bold;'>What is the statistical power 
                                                                   in a randomized complete block design?"),
                                                                status = "primary", solidHeader = TRUE,
                                                                
                                                                h4(HTML("In the case of RCBD, the objective is to determine
                                                                          the number of blocks associated with the probability
                                                                          of detecting significant differences in the treatments.
                                                                          <br><br>
                              
                                                                          The power associated with the RCBD has a
                                                                          theoretical probability distribution given by:
                              
                                                                          $$F_{(t-1),(r*t*b-1)(b-1)(t-1),\\lambda}$$
                              
                                                                          Where \\(t\\) is the number of treatments, \\(b\\) is the
                                                                          number of blocks, and \\(r\\) the number of replicates per treatment in
                                                                          each block.<br><br>
                              
                                                                          The non-centrality parameter for the F test associated
                                                                          with detecting significant differences in treatments
                                                                          for RBD is:
                              
                                                                          $$\\lambda = \\displaystyle\\frac{r \\times b}{\\sigma^2}\\displaystyle\\sum_{j=1}^{t}(\\mu_j-\\bar \\mu)^2$$

                                            "))
                                                            ))
                                                   ,
                                                   ##########################################################################
                                                   tabPanel(HTML('Power for a given <br> combination of blocks <br> and replicates'),
                                                            box(width = 6,title = HTML("<span style='color:white; font-weight:bold;'>Power for a given number of blocks and replicates"),
                                                                status = "primary", solidHeader = TRUE,
                                                                sliderInput('NumBlocks','Choose a number of blocks',value=3,min=2,max=20,step=1),
                                                                plotlyOutput('PlotPowerByBlocks'),
                                                                hr(),
                                                                h5("Blue line: CRD; Red line: RCBD")),
                                                            box(width=6,title = HTML("<span style='color:white; font-weight:bold;'>Power and number of blocks"),
                                                                status = "primary", solidHeader = TRUE,
                                                                includeHTML("www/HTML_files/Power_and_Number_of_Blocks.html")
                                                            )
                                                   )
                                                   
                                                   
                                                   ,
                                                   tabPanel('Optimal design',
                                                            numericInput('target_power','Desired minimum power',value=0.8),
                                                            numericInput('MaxDesigns','Number of desired designs',value=10),
                                                            tableOutput('OptimalDesign')),
                                                   #####################################################33
                                                   tabPanel(HTML('Empirical <br>power'),
                                                            box(width=12,title = HTML("<span style='color:white; font-weight:bold;'>Explanation of How the Empirical 
                                                                 Power Is Obtained"),
                                                                status = "primary", solidHeader = TRUE,
                                                                h4(HTML("Power is the probability of detecting the treatment effect when it truly exists.
                                                                         The plot compares:<br><br>
                                                                         <b>Theoretical power</b>, calculated from the non-central F-distribution using
                                                                         the expected treatment effects and residual variance. Empirical (simulated) power, estimated
                                                                         by repeatedly simulating experiments and performing the F-test on each one.<br><br>
                                                                         <b>How the empirical power is computed:</b> We specify the true treatment differences and block effects.
                                                                         We repeatedly (e.g., 2000 times) simulate data from the RCBD model. For each simulated dataset, we fit
                                                                         the ANOVA model and compute the F statistic for the treatment effect. We check how often the F statistic
                                                                         exceeds the critical value for a chosen significance level (e.g., α = 0.05). The proportion of simulations
                                                                         where the null hypothesis is rejected is the empirical power.<br><br>
                                                                         The histogram in the plot shows the distribution of F values obtained from the simulations.
                                                                         The dotted curve shows the F distribution under the null hypothesis (no treatment effect).
                                                                         The shaded/red area represents the F distribution when the true treatment effects are present.
                                                                         The overlap between them determines the power: less overlap → higher power.
                                                                                 The close match between the theoretical and empirical power confirms that the power calculation
                                                                                 is behaving as expected."))
                                                            ),
                                                            box(width=12,title = HTML("<span style='color:white; font-weight:bold;'>Empirical power 
                                                                 considering the parameters of the RCBD"),
                                                                status = "primary", solidHeader = TRUE,
                                                                
                                                                h4(HTML("The dotted curve shows the F-distribution under the null hypothesis
                                                                          (no treatment effect). The histogram and red curve show the distribution of the observed F
                                                                          statistics from the simulated experiments when the treatment effect is present.
                                                                          The proportion of simulated F values that exceed the critical value gives the empirical power.
                                                                          The reported theoretical power is obtained from the non-central F-distribution using the specified effect size.
                                                                                  ")),
                                                                fluidRow(
                                                                  column(3,
                                                                         numericInput('num_sim',
                                                                                      withMathJax('Number of simulations'),
                                                                                      value=2000)),
                                                                  column(2,
                                                                         numericInput('binwSimPower',"Binwidth",value=1)),
                                                                  column(2,
                                                                         numericInput('ScalePowerSim','Max value for density',
                                                                                      value=0.2)),
                                                                  column(2,
                                                                         numericInput('ScaleFSim','Max value of F',value=50))),
                                                                column(12,
                                                                       h4("Power for RCBD"),
                                                                       hr(),
                                                                       withSpinner(plotOutput("EmpiricalPower_RCBD"), type = 4)
                                                                )
                                                                
                                                            ))
                                       )),
                              #########################################################
                              tabPanel(HTML("Precission on <br>effect's estimation"),
                                       fluidPage(
                                         column(6,
                                                box(width=12,title = HTML("<span style='color:white; font-weight:bold;'>Estimate treatment effects"),
                                                    status = "success", solidHeader = TRUE,
                                                h4(HTML("The following results show the precision of
                                                         the confidence intervals by the Tukey method
                                                         for the difference of treatment effects"))),
                                                box(width=12,title = HTML("<span style='color:white; font-weight:bold;'>CI for the difference of treatment effects"),
                                                    status = "primary", solidHeader = TRUE,
                                                h3(HTML("Estimated Effect for difference in treatment effects")),
                                                numericInput('Conf.CI.Tukey','Confidence of CI',value=0.95),
                                                tableOutput('SampleSizeTukey')
                                         )),
                                         column(6,
                                                box(width=12,title = HTML("<span style='color:white; font-weight:bold;'>Required sample size for precission"),
                                                    status = "success", solidHeader = TRUE,
                                                h4(HTML("Required sample size (number of blocks and replicates) determined
                                                to attain a desired precision of the CI for the difference of
                                                        treatment effects.")), hr(),
                                                h4(uiOutput('PrecisionTheoretical')),
                                                hr(),
                                                numericInput('DesiredPrecission','Desired CI precission (minimum effect to detect)',value=1,min=0.1,step=0.1),
                                                hr(),
                                                radioButtons('OptionPrecission','Which is your option',
                                                             choices=c('Increase blocks'='IncBlocks',
                                                                       'Increase replicates'='IncReplicates')),
                                                uiOutput('DesiredPrecissionTukey'))
                                                )) )
                            )))
                            ),
                    ########################################################################################

                    #######################################################################################
                    tabItem(
                      tabName = "ExploreCases",
                      fluidPage(
                        h2("Here are some suggested problems to explore using this app"),
                        hr(),
                        radioButtons('SuggestedCases','Select on case:',
                                     choices=c("Multicentric evaluation of treatments"="Multicentric",
                                               "Multicentric evaluation of treatments 2"="Multicentric2")),
                        conditionalPanel(condition="input.SuggestedCases=='Multicentric'",
                                         includeHTML("www/HTML_files/multicenter_rcbd_exercise.html")),
                        conditionalPanel(condition="input.SuggestedCases=='Multicentric2'",
                        box(width=12,title = HTML("<span style='color:white; font-weight:bold;'>A multicentric clinical trial"),
                            status = "primary", solidHeader = TRUE,
                            h4(HTML("Consider a multicentric clinical trial
                               for evaluating the mean concentration of a metabolite after three different
                               treatments. The mean value for the first treatment, which is a well characterized
                               one, is 68 mg/dL , with a standard deviation of 8.3mg/dL. We want to design the trial so that we can
                               estimate the difference between treatments considering the minimum differences
                               to be \\(\\tau_1=0,\\tau_2=4,\\tau_3=6\\).  The trial is conducted in four hospitals in different
                               locations and will be considered as blocks to control for variability due to
                               population differences. We will consider a medium effect of hospitals.
                               We start with 6 subjects per disease in each hospital.<br>
                               <br>
                                    <ul>
                                    <li> Do we have enough statistical power to identify the effects?</li>
                                    <li> Under these parameters, which is the expected precision in estimating
                                    the difference among treatment responses?</li>
                                    <li> Choose an appropriate precision and decide what do you need to change in your trial
                                    to attain such precision </li>
                                    <li> After deciding the appropiate design, run several experiments and evaluate the outcomes.
                                    Are you satisfied with the simulation's results? Do they call for futher changes in your design?</li>
                                    <li> Redo the previous computations in the case \\(\\tau_1=0,\\tau_2=3,\\tau_3=4\\).
                                    Comment the differences</li>
                                    </ul>"))
                            ))
                      )


                    ),
                    tabItem("Contact")

                    ############################ CLOSING #########################################################

                  ) ### End of tabItems
                  
            )       ### End of dashboardBody
            
       )            ### End of dashboardPage
  
  )                 ### End of UI ###################################################################################
                  

                
                










