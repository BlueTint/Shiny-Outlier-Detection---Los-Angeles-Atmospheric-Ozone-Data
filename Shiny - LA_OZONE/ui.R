### ui.R ###

shinyUI(navbarPage(theme = shinytheme("sandstone"),
                   htmlOutput("title"),
                   # Overview UI
                   tabPanel(h6(strong("Overview")),
                            sidebarPanel(h3(strong("OZONE DATA")),
                                         h6("Outlier Detection Suite Authored By Phillip Gao"),
                                         hr(),
                                         h4(strong("Purpose of Application")),
                                         h5("This application functions as a method suite used in the detection of outliers through the use of a variety of statistical techniques"),
                                         radioButtons(inputId = "OverviewBox", 
                                                      label = "",
                                                      choices = c("Summary", "Data Table"), inline = TRUE, selected="Summary"),
                                         hr(),
                                         h4(strong("Data Description")),
                                         h5("This dataset serves as the daily recorded measurements of atmospheric ozone concentration from eight meteorological measurements that were in the Los Angeles basin for 330 days of 1976."),
                                         h5("The response, identified as the 'upo3' or Atmospheric ozone, is the daily recorded maximum of the hourly-average conc. in Upland California"),
                                         hr(),
                                         h4(strong("Data Format")),
                                         h5("A data frame containing 330 observations on the following variables."),
                                         h5(HTML('&nbsp;'), HTML('&nbsp;'), strong("upo3"), HTML('&nbsp;'), HTML('&nbsp;'), "Upland, CA Maximum ozone concentration (PPM)"),
                                         h5(HTML('&nbsp;'), HTML('&nbsp;'), strong("vdht"), HTML('&nbsp;'), HTML('&nbsp;'),"Vandenberg 500 millibar height (M)"),
                                         h5(HTML('&nbsp;'), HTML('&nbsp;'), strong("wdsp"), HTML('&nbsp;'), HTML('&nbsp;'),"Wind speed (MPH)"),
                                         h5(HTML('&nbsp;'), HTML('&nbsp;'), strong("hmdt"), HTML('&nbsp;'), HTML('&nbsp;'),"Humidity (Percentage)"),
                                         h5(HTML('&nbsp;'), HTML('&nbsp;'), strong("sbtp"), HTML('&nbsp;'), HTML('&nbsp;'),"Sandburg Airforce Base temperature (Celsius)"),
                                         h5(HTML('&nbsp;'), HTML('&nbsp;'), strong("ibht"), HTML('&nbsp;'), HTML('&nbsp;'),"Inversion base height (foot)"),
                                         h5(HTML('&nbsp;'), HTML('&nbsp;'), strong("dgpg"), HTML('&nbsp;'), HTML('&nbsp;'),"Daggot pressure gradient (mmHg)"),
                                         h5(HTML('&nbsp;'), HTML('&nbsp;'), strong("ibtp"), HTML('&nbsp;'), HTML('&nbsp;'),"Inversion base temperature (Fahrenheit)"),
                                         h5(HTML('&nbsp;'), HTML('&nbsp;'), strong("vsty"), HTML('&nbsp;'), HTML('&nbsp;'),"Visibility (miles)"),
                                         h5(HTML('&nbsp;'), HTML('&nbsp;'), strong("day"), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'),"Calendar day of year (1976)")),
                            mainPanel(
                                hr(),
                                verbatimTextOutput(outputId = "Summary"),
                                verbatimTextOutput(outputId = "Glimpse"),
                                DT::dataTableOutput(outputId = "DataTable")
                            ),
                   ),
                   # Univariable outliers UI
                   tabPanel(h6(strong("Univariable outliers")),
                            sidebarPanel(
                              h3(strong("BOXPLOTS")),
                              h5("Boxplots serve as an indication of the variability or dispersion of data, providing standardized distribution display through the quartile or percentile summaries"),
                              h5("The quartile or percentile partitioning aides in the identification of potential outliers and its severity using boxplots."),
                              hr(),
                              
                              selectInput(inputId = "Variables", label = "Choose variables", 
                                          choices = c("upo3", "vdht", "wdsp", "hmdt", "sbtp", "ibht", "dgpg", "ibtp", "vsty"), 
                                          selected = c("upo3", "vdht", "wdsp", "hmdt", "sbtp", "ibht", "dgpg", "ibtp", "vsty"),
                                          multiple = TRUE),
                              checkboxInput(inputId = "Outliers", label = "Show Outliers", value = TRUE),
                              h6("Shows/Hides identified outlier observations"),
                              hr(),
                              checkboxInput(inputId = "Standardise", label = "Show Standardized", value = TRUE),
                              h6("Standardisation enables us to understand the data regardless its size"),
                              hr(),
                              checkboxInput(inputId = "PowerTransform", label = "Power-transformed", value = TRUE),
                              h6("Yeo-Johnson transformation transforms the data so as to improve normality."),
                              hr(),
                              sliderInput(inputId = "Range", label = "IQR Multiplier", min = 0.0, max = 9.0, value = 1.5, step = 0.1),
                              ),
                            mainPanel(
                              h4(strong("BOXPLOTS of VARIABLES")),
                              plotOutput(outputId = "Boxplot")
                            ) 
                   ),
                   
                   # Multivariable outliers UI
                   tabPanel(h6(strong("Multivariable outliers")),
                            sidebarPanel(
                              h3(strong("MAHALANOBIS DISTANCE")),
                              h5("Mahalanobis distance can determine multivariate outliers. 
                                 Points which are beyond Mahalanobis distance threshold are regarded as outliers"),
                              hr(),
                              
                              checkboxInput(inputId = "YJMahalanobis", label = "Power-transformed", value = TRUE),
                              h6("Yeo-Johnson transformation transforms the data so as to improve normality."),
                              hr(),
                              
                              sliderInput(inputId = "MahalanobisThreshold", label = "Mahalanobis Distance Threshold",
                                          min = 0.0, max = 0.999, value = 0.9, step = 0.01),
                              hr(),
                              
                              h5(strong("Summary of Outliers")),
                              verbatimTextOutput(outputId = "Summary_Mahalanobis")
                              ),
                            mainPanel(
                              h4(strong("OUTLIER PATTERN")),
                              h5(strong("*Outlier labels indicate 'day'.")),
                              plotOutput(outputId = "Mahalanobis"),
                              h4(strong("OUTLIERS")),
                              DT::dataTableOutput(outputId = "MahalanobisTable")
                            ),
                   ),
                   
                   # Model based outliers UI
                   tabPanel(h6(strong("Model based outliers")),
                            tabsetPanel(
                              # Cook's distance UI
                              tabPanel(strong("COOK'S DISTANCE"),
                                       sidebarPanel(
                                         h3(strong("COOK'S DISTANCE")),
                                         h5("Cook's distance can measure how much observations affect to the model.
                                            Points which are over the threshold of Cook's distance can be seen as outliers."),
                                         hr(),
                                         checkboxInput(inputId = "YJCook", label = "Power-transformed", value = TRUE),
                                         h6("Yeo-Johnson transformation transforms the data so as to improve normality."),
                                         hr(),
                                         
                                         sliderInput(inputId = "CookThreshold", label = "Cook Distance Threshold",
                                                     min = 0.1, max = 9.0, value = 3, step = 0.1),
                                         hr(),
                                         
                                         h5(strong("Summary of Outliers")),
                                         verbatimTextOutput(outputId = "Summary_Cook")
                                         ),
                                       mainPanel(
                                         h4(strong("OUTLIER PATTERN")),
                                         h5(strong("*Outlier labels indicate 'day'.")),
                                         plotOutput(outputId = "Cook"),
                                         h4(strong("OUTLIERS")),
                                         DT::dataTableOutput(outputId = "CookTable")
                                         )
                                       ),
                              # DBScan UI
                              tabPanel(strong("DBScan"),
                                       sidebarPanel(
                                         h3(strong("DBSCAN")),
                                         h5("Density-based spatial clustering of applications with noise (DBSCAN)
                                         is based on a set of points. Points that are close to each other are categorized
                                         with a same group according to distance measurement and a minimum number of points.
                                         Points that are in low-density regions can regarded as outliers."),
                                         hr(),
                                         
                                         selectInput("Variables_DBSCAN", label = "Choose variables to be considered for plotting",
                                                     choices = c("vdht", "wdsp", "hmdt", "sbtp", "ibht", "dgpg", "ibtp", "vsty"),
                                                     selected = c("vdht", "wdsp", "hmdt", "sbtp", "ibht", "dgpg", "ibtp", "vsty"),
                                                     multiple = TRUE),
                                         hr(),
                                         
                                         sliderInput(inputId = "minPts", label = "Minimum number of points for clustering",
                                                     min = 1.0, max = 12.0, value = 9, step = 1),
                                         hr(),
                                         
                                         sliderInput(inputId = "epsValue", label = "Value for the eps parameter",
                                                     min = 10.0, max = 730.0, value = 360, step = 5),
                                         h5("The level of eps determines the distance between points"),
                                         hr(),
                                         
                                         h5(strong("Summary of Outliers")),
                                         verbatimTextOutput(outputId = "Summary_dbscan")
                                         ),
                                       mainPanel(
                                         h4(strong("K-DISTANCE PLOT")),
                                         h5(strong("*From K-distance plot we can estimate an appropriate eps value.")),
                                         h5(strong("The point where the graph rises drastically is usually considered.")),
                                         plotOutput(outputId = "KNNdist"),
                                         hr(),
                                         h4(strong("CLUSTER PLOT")),
                                         h5(strong("*Outlier labels in red indicate 'day'.")),
                                         plotOutput(outputId = "DBscanPlot"),
                                         hr(),
                                         hr(),
                                         h4(strong("OUTLIERS")),
                                         DT::dataTableOutput(outputId = "DBscanTable")
                                         )
                                       ),
                              
                              # Local outlier factors UI
                              tabPanel(strong("Local outlier factors"),
                                       sidebarPanel(
                                         h3(strong("LOCAL OUTLIER FACTORS")),
                                         h5(" Local Outlier factors is devised to find the likelihood that observations
                                            are outliers by measuring the local deviation of a given data point regarding
                                            its neighbouring points. If the 'reachability distance' by LOF is greater than 1,
                                            it is worth investigating."),
                                         hr(),
                                         
                                         sliderInput(inputId = "lofMinPts", label = "Minimum number of points for clustering",
                                                     min = 1.0, max = 12.0, value = 9, step = 1),
                                         hr(),
                                         
                                         sliderInput(inputId = "lofThreshold", label = "Value for the eps parameter",
                                                     min = 0.0, max = 3.0, value = 1.5, step = 0.1),
                                         hr(),
                                         
                                         h5(strong("Summary of Outliers")),
                                         verbatimTextOutput(outputId = "Summary_lof")
                                         ),
                                       mainPanel(
                                         h4(strong("OUTLIERS BY LOCAL OUTLIER FACTOR")),
                                         DT::dataTableOutput(outputId = "lofTable")
                                         )
                                       ),
                              
                              # Support vector machines  UI
                              tabPanel(strong("Support vector machines"),
                                       sidebarPanel(
                                         h3(strong("SUPPORT VECTOR MACHINES")),
                                         h5("Support vector machine can also detect outliers by optimising nu value. 
                                            Nu parameter controls training errors."),
                                         hr(),
                                         
                                         sliderInput(inputId = "svmNu", label = "Set a parameter to determine support vectors",
                                                     min = 0.01, max = 1.0, value = 0.1, step = 0.01),
                                         hr(),
                                         
                                         h5(strong("Summary of Outliers")),
                                         verbatimTextOutput(outputId = "Summary_svm")
                                         ),
                                       mainPanel(
                                         h4(strong("OUTLIERS BY SUPPORT VECTOR MACHINES")),
                                         DT::dataTableOutput(outputId = "svmTable")
                                         )
                                       )
                              )
                            ),
                   
                   # Robust methods UI
                   tabPanel(h6(strong("Robust method")),
                            sidebarPanel(
                              h3(strong("QUANTILE REGRESSION FOREST")),
                              
                              selectInput("Variables_qrf", label = "Choose variables to be considered for plotting",
                                          choices = c("vdht", "wdsp", "hmdt", "sbtp", "ibht", "dgpg", "ibtp", "vsty"),
                                          selected = c("vdht", "wdsp", "hmdt", "sbtp", "ibht", "dgpg", "ibtp", "vsty"),
                                          multiple = TRUE),
                              h5("Quantile Regression Forest is a tree-based ensemble method to estimate the conditional 
                                 quantiles. It gives a non-parametric and accurate way of estimating conditional quantiles 
                                 for high-dimensional predictors. Besides, QRF can be used for outlier detection 
                                 by setting a quantile as a criterion"),
                              hr(),
                              
                              sliderInput(inputId = "quantile", label = "Quantile level",
                                          min = 0.01, max = 1.0, value = 0.9, step = 0.01),
                              h5("The number of outliers varies based on which conditional quantile is arranged"),
                              hr(),
                              
                              sliderInput(inputId = "nodesize", label = "Number of node used in training",
                                          min = 1, max = 100, value = 10, step = 1),
                              h5("A target for the minimum number of observations in each tree leaf."),
                              hr(),
                              
                              sliderInput(inputId = "sampsize", label = "Number of sample used in training",
                                          min = 1, max = 230, value = 30, step = 1),
                              hr(),
                              
                              h5(strong("Summary of Outliers")),
                              verbatimTextOutput(outputId = "Summary_qrf"),
                              ),
                            
                            mainPanel(
                              #plotOutput(outputId = "qrfPlot"),
                              DT::dataTableOutput(outputId = "qrfTable")
                                )
                            )
                   )
        )
