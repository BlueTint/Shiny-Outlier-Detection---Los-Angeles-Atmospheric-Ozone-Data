### server.R ###

shinyServer(function(input, output) {
    
    output$title <- renderText({
        paste0("<p>", "OUTLIER DETECTION SUITE", "</p>", h5("Los Angeles Atmospheric Ozone Concentration"))
    })
    
    #  Overview tab
    output$Summary <- renderPrint({
        if (input$OverviewBox == "Summary") {
            summary(data)
        }
    })
    
    output$Glimpse <- renderPrint({
        if (input$OverviewBox == "Summary") {
            glimpse(data)
        }
    })
    
    output$DataTable <- DT::renderDataTable({
        if (input$OverviewBox == "Data Table") {
            DT::datatable(data = data)
        }
    })
    
    # Univariable outliers tab
    output$Boxplot <- renderPlot({
        if (input$PowerTransform == FALSE) {
          numericCols <- unlist(lapply(data_te, is.numeric)) 
          std <- scale(data_te[,numericCols, drop = FALSE], center = input$Standardise, scale = input$Standardise)
          d <- tidyr::gather(as.data.frame(std)[,input$Variables])
          ggplot(
            mapping = aes(x = d$key, y = d$value, fill = d$key, label = d$value)) + 
            geom_boxplot(coef = input$Range, outlier.colour = "red", outlier.alpha = input$Outliers) +
            labs(title = paste("Uni-variable boxplots at IQR multiplier of", input$Range),
                 x = "Standardised variable value", y = "Std Value") +
            coord_flip() +  geom_text_repel(
              nudge_y      = 0.05,
              direction    = "x",
              angle        = 90,
              vjust        = 0,
              segment.size = 0.2
            ) 
        }
        
        else if (input$PowerTransform == TRUE) {
          numericCols <- unlist(lapply(yj_te, is.numeric)) 
          std <- scale(yj_te[,numericCols, drop = FALSE], center = input$Standardise, scale = input$Standardise)
          d <- tidyr::gather(as.data.frame(std)[,input$Variables])
          ggplot(mapping = aes(x = d$key, y = d$value, fill = d$key)) + 
            geom_boxplot(coef = input$Range, outlier.colour = "red", outlier.alpha = input$Outliers) +
            labs(title = paste("Uni-variable boxplots at IQR multiplier of", input$Range),
                 x = "Standardised variable value", y = "Std Value") +
            coord_flip()
        }
    })
    
    # Multivariable outliers tab - Mahalanobis
    output$Mahalanobis <- renderPlot({
        if (input$YJMahalanobis == FALSE) {
            varMat <- var(without_yj_te)
            colM <- colMeans(without_yj_te)
            md2 <- mahalanobis(x = without_yj_te, center = colM, cov = varMat)
            threshold <- qchisq(p = input$MahalanobisThreshold, df = ncol(without_yj_te))
            without_yj_te$outlier <- ifelse(threshold < md2, without_yj_te$day, as.numeric(NA))
            ggplot(mapping = aes(y = md2, x = (1:length(md2))/length(md2))) +
                geom_point() +
                scale_y_continuous(limits = c(0, max(md2)*1.1)) +
                labs(y = "Mahalanobis distance squared", x = "Complete Observations") +
                geom_abline(slope = 0, intercept = threshold, color = "red") +
                scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0%", "50%", "100%")) +
                geom_label_repel(aes(label = without_yj_te$outlier), na.rm = TRUE, nudge_y = 1, check_overlap = TRUE) +
                theme(legend.position = "bottom")
        }
        
        else if (input$YJMahalanobis == TRUE) {
            varMat <- var(yj_te)
            colM <- colMeans(yj_te)
            md2 <- mahalanobis(x = yj_te, center = colM, cov = varMat)
            threshold <- qchisq(p = input$MahalanobisThreshold, df = ncol(yj_te)) 
            yj_te$outlier <- ifelse(threshold < md2, without_yj_te$day, as.numeric(NA))
            ggplot(mapping = aes(y = md2, x = (1:length(md2))/length(md2))) +
                geom_point() +
                scale_y_continuous(limits = c(0, max(md2)*1.1)) +
                labs(y = "Mahalanobis distance squared", x = "Complete Observations") +
                geom_abline(slope = 0, intercept = threshold, color = "red") +
                scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0%", "50%", "100%")) +
                geom_label_repel(aes(label = yj_te$outlier), na.rm = TRUE, nudge_y = 1, check_overlap = TRUE) +
                theme(legend.position = "bottom")
        }
    })
    
    output$MahalanobisTable <- DT::renderDataTable({
        if (input$YJMahalanobis == FALSE) {
            varMat <- var(without_yj_te) 
            colM <- colMeans(without_yj_te)
            md2 <- mahalanobis(x = without_yj_te, center = colM, cov = varMat)
            threshold <- qchisq(p = input$MahalanobisThreshold, df = ncol(without_yj_te)) 
            without_yj_te$outlier <- ifelse(threshold < md2, without_yj_te$day, as.numeric(NA))
            DT::datatable(data = without_yj_te[is.na(without_yj_te$outlier) == FALSE,][1:10])
        }
        
        else if (input$YJMahalanobis == TRUE) {
            varMat <- var(yj_te) 
            colM <- colMeans(yj_te) 
            md2 <- mahalanobis(x = yj_te, center = colM, cov = varMat)
            threshold <- qchisq(p = input$MahalanobisThreshold, df = ncol(yj_te)) 
            yj_te$outlier <- ifelse(threshold < md2, yj_te$day, as.numeric(NA))
            DT::datatable(data = yj_te[is.na(yj_te$outlier) == FALSE,][1:10])
        }
    })
    
    output$Summary_Mahalanobis <- renderPrint({
      if (input$YJMahalanobis == FALSE) {
        varMat <- var(without_yj_te) 
        colM <- colMeans(without_yj_te)
        md2 <- mahalanobis(x = without_yj_te, center = colM, cov = varMat)
        threshold <- qchisq(p = input$MahalanobisThreshold, df = ncol(without_yj_te)) 
        without_yj_te$outlier <- ifelse(threshold < md2, without_yj_te$day, as.numeric(NA))
        summary(without_yj_te[is.na(without_yj_te$outlier) == FALSE,][1:10])
      }
      
      else if (input$YJMahalanobis == TRUE) {
        varMat <- var(yj_te) 
        colM <- colMeans(yj_te) 
        md2 <- mahalanobis(x = yj_te, center = colM, cov = varMat)
        threshold <- qchisq(p = input$MahalanobisThreshold, df = ncol(yj_te)) 
        yj_te$outlier <- ifelse(threshold < md2, yj_te$day, as.numeric(NA))
        summary(yj_te[is.na(yj_te$outlier) == FALSE,][1:10])
      }
    })
    
    # Model based outliers tab - Cook Distance
    output$Cook <- renderPlot({
        if (input$YJCook == FALSE) {
            lmod <- glm(formula = upo3 ~ ., data = without_yj_te, family = gaussian)
            dc <- cooks.distance(lmod)
            thresh <- input$CookThreshold * mean(dc)
            dfcd <- data.frame(dc, id = 1:length(dc)/length(dc))
            without_yj_te$outlier <- ifelse(thresh < dc, without_yj_te$day, as.numeric(NA))
            ggplot(data = dfcd, mapping = aes(y = dc, x = id)) +
                geom_point() +
                scale_y_continuous(limits = c(0, max(dfcd$dc)*1.1)) +
                labs(y = "Cook's distance", x = "Complete Observations") +
                geom_abline(slope = 0, intercept = thresh, color = "red") +
                scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0%", "50%", "100%")) +
                geom_label_repel(aes(label = without_yj_te$outlier), na.rm = TRUE, nudge_y = 0, check_overlap = TRUE) +
                theme(legend.position = "bottom")
        }
        
        
        else if (input$YJCook == TRUE) {
            lmod <- glm(formula = upo3 ~ ., data = yj_te, family = gaussian)
            dc <- cooks.distance(lmod)
            thresh <- input$CookThreshold * mean(dc) 
            dfcd <- data.frame(dc, id = 1:length(dc)/length(dc))
            yj_te$outlier <- ifelse(thresh < dc, yj_te$day, as.numeric(NA))
            ggplot(data = dfcd, mapping = aes(y = dc, x = id)) +
                geom_point() +
                scale_y_continuous(limits = c(0, max(dfcd$dc)*1.1)) +
                labs(y = "Cook's distance", x = "Complete Observations") +
                geom_abline(slope = 0, intercept = thresh, color = "red") +
                scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0%", "50%", "100%")) +
                geom_label_repel(aes(label = yj_te$outlier), na.rm = TRUE, nudge_y = 0, check_overlap = TRUE) +
                theme(legend.position = "bottom")
        }
    })
    
    output$CookTable <- DT::renderDataTable({
        if (input$YJCook == FALSE) {
            lmod <- glm(formula = upo3 ~ ., data = without_yj_te, family = gaussian)
            dc <- cooks.distance(lmod)
            thresh <- input$CookThreshold * mean(dc)
            without_yj_te$outlier <- ifelse(thresh < dc, without_yj_te$day, as.numeric(NA))
            DT::datatable(data = without_yj_te[is.na(without_yj_te$outlier) == FALSE,][1:10])
        }
        
        else if (input$YJCook == TRUE) {
            lmod <- glm(formula = upo3 ~ ., data = yj_te, family = gaussian)
            dc <- cooks.distance(lmod)
            thresh <- input$CookThreshold * mean(dc)
            yj_te$outlier <- ifelse(thresh < dc, yj_te$day, as.numeric(NA))
            DT::datatable(data = yj_te[is.na(yj_te$outlier) == FALSE,][1:10])
        }
        
    })
    
    output$Summary_Cook <- renderPrint({
      if (input$YJCook == FALSE) {
        lmod <- glm(formula = upo3 ~ ., data = without_yj_te, family = gaussian)
        dc <- cooks.distance(lmod)
        thresh <- input$CookThreshold * mean(dc)
        without_yj_te$outlier <- ifelse(thresh < dc, without_yj_te$day, as.numeric(NA))
        DT::datatable(without_yj_te[is.na(without_yj_te$outlier) == FALSE,][1:10])
        summary(without_yj_te[is.na(without_yj_te$outlier) == FALSE,][1:10])
      }
      
      else if (input$YJCook == TRUE) {
        lmod <- glm(formula = upo3 ~ ., data = yj_te, family = gaussian)
        dc <- cooks.distance(lmod)
        thresh <- input$CookThreshold * mean(dc)
        yj_te$outlier <- ifelse(thresh < dc, yj_te$day, as.numeric(NA))
        DT::datatable(data = yj_te[is.na(yj_te$outlier) == FALSE,][1:10])
        summary(yj_te[is.na(yj_te$outlier) == FALSE,][1:10])
      }
    })
    
    # Model based outliers tab - DBSCAN
    output$KNNdist <- renderPlot({
        dbscan::kNNdistplot(without_yj_te, k = input$minPts) + abline(h = input$epsValue, lty = 3)
    })
    
    output$DBscanPlot <- renderPlot({
        clustered <- dbscan::dbscan(without_yj_te, eps = input$epsValue, minPts = input$minPts)
        rownames(without_yj_te) <- without_yj_te$day
        fviz_cluster(clustered, without_yj_te, stand = TRUE, ellipse = TRUE,
                     choose.vars = input$Variables_DBSCAN,
                     pointsize = 0.5, labelsize = 7,
                     outlier.labelsize = 12, 
                     repel = TRUE, star.plot = T,
                     geom = c("point", "text"),
                     title ="",
                     show.clust.cent = FALSE, outlier.color = "red", outlier.shape = 19, outlier.pointsize = 3.5)
    })
    
    output$DBscanTable <- DT::renderDataTable({
        clustered <- dbscan::dbscan(without_yj_te, eps = input$epsValue, minPts = input$minPts)
        DT::datatable(data = without_yj_te[clustered$cluster == 0,])
    })
    
    output$Summary_dbscan <- renderPrint({
      clustered <- dbscan::dbscan(without_yj_te, eps = input$epsValue, minPts = input$minPts)
      summary(without_yj_te[clustered$cluster == 0,])
      })
    
    # Model based outliers tab - Local Factor Outliers
    output$lofTable <- DT::renderDataTable({
        without_yj_te_MX <- as.matrix(without_yj_te[, 2:9]) # Only selects predictor variables excluding response variable 'upo3'
        d <- dbscan::lof(without_yj_te_MX, k = input$lofMinPts)
        without_yj_te$distance <- d
        without_yj_te <- without_yj_te[order(d, decreasing = TRUE),]
        without_yj_te[without_yj_te$distance > input$lofThreshold,]
    })
    
    output$Summary_lof <- renderPrint({
      without_yj_te_MX <- as.matrix(without_yj_te[, 2:9]) # Only selects predictor variables excluding response variable 'upo3'
      d <- dbscan::lof(without_yj_te_MX, k = input$lofMinPts)
      without_yj_te$distance <- d
      without_yj_te <- without_yj_te[order(d, decreasing = TRUE),]
      summary(without_yj_te[without_yj_te$distance > input$lofThreshold,])
    })
    
    # Model based outliers tab - SVM 
    output$svmTable <- DT::renderDataTable({
        without_yj_te_MX <- as.matrix(without_yj_te[, 2:9]) # Only selects predictor variables excluding response variable 'upo3'
        model <- e1071::svm(without_yj_te_MX, y = without_yj_te[, 10], type = 'one-classification', nu = input$svmNu, scale = TRUE, kernel = "radial")
        good <- predict(model, without_yj_te_MX)
        without_yj_te[!good,]
    })
    
    output$Summary_svm <- renderPrint({
      without_yj_te_MX <- as.matrix(without_yj_te[, 2:9]) # Only selects predictor variables excluding response variable 'upo3'
      model <- e1071::svm(without_yj_te_MX, y = without_yj_te[, 10], type = 'one-classification', nu = input$svmNu, scale = TRUE, kernel = "radial")
      good <- predict(model, without_yj_te_MX)
      summary(without_yj_te[!good,])
    })
    
    # Robust Method - QRF
    output$qrfTable <- DT::renderDataTable({
        qrf <- quantregForest(x = data_tr[,input$Variables_qrf], y = data_tr$upo3, nodesize = input$nodesize, sampsize = input$sampsize, keep.inbag = TRUE)
        conditionalQuantiles <- predict(qrf, data_tr[2:9],  what=0.01*(1:100), keep.inbag = TRUE)
        data_tr$outlier <- ifelse(data_tr$upo3 > conditionalQuantiles[,input$quantile*100], data_tr$day, as.numeric(NA))
        DT::datatable(data = data_tr[is.na(data_tr$outlier) == FALSE,][,c(10,1,2,3,4,5,6,7,8,9)])
    })
    
    output$Summary_qrf <- renderPrint({
      qrf <- quantregForest(x = data_tr[,input$Variables_qrf], y = data_tr$upo3, nodesize = input$nodesize, sampsize = input$sampsize, keep.inbag = TRUE)
      conditionalQuantiles <- predict(qrf, data_tr[2:9],  what=0.01*(1:100), keep.inbag = TRUE)
      data_tr$outlier <- ifelse(data_tr$upo3 > conditionalQuantiles[,input$quantile*100], data_tr$day, as.numeric(NA))
      summary(data_tr[is.na(data_tr$outlier) == FALSE,][,c(10,1,2,3,4,5,6,7,8,9)])
    })
})