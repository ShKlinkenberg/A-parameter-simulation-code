# Clear memory
rm(list = ls())

set.seed(2535)

# Load mathgarden multiplication data
load("real_data/datDomain3time1.Rdata")

# Load item content
load("item_clusters_heatmap/items_domain3_apar.Rdata")

# Select item with highest frequency
# calculate item frequencies
# item.frequency <- aggregate(new_user_domain_modified_count ~ item_id, dat, length)
# Select maximum frequency
# highest.frequency <- max(item.frequency$new_user_domain_modified_count)
# Select item with maximum frequency
# item <- item.frequency[item.frequency$new_user_domain_modified_count == highest.frequency,'item_id']
# 
# item <- item.highest.frequency

# Select random item and create PDF
item <- 881
pdf("analyse_proportion_correct/Proportion_correct_2_models.pdf", width = 7, height = 3.5)

# All items to create GIF
# item <- unique(dat$item_id)
# png(file="analyse_proportion_correct/temp_proportio_correct_image_sequence/example%02d.png", width=500, height=300)

for (i in item) {
  
  # Select subset of data for current item i
  data <- subset(dat, item_id == i )
  
  # Calculate delta, difference between theta and beta
  data$delta <- data$user_domain_rating - data$item_rating
  # data$delta <- data$user_domain_rating - mean(data$item_rating)
  
  # Center to .5 at theta - beta = 0
  # data$delta <- as.numeric( scale(data$delta) )
  # Nope, standardising doesn't work
  
  # Subset to only delta and correct answers
  data <- data[,c('delta', 'correct_answered')]
  
  # Create bins for plotting proportions correct
  data$bins <- round(data$delta,1)
  
  #item
  question <- items[items$id == i, "question"]
  
  # Plot correct by delta
  plot(data$bins,
       data$correct_answered,
       main = paste("Proportion correct on item:", question),
       pch  = 15,
       cex  = 2,
       col  = ifelse(data$correct_answered==1,rgb(0, 1, 0, 0.01),  # Green if correct 1
                                              rgb(1, 0, 0, 0.01)), # Red if incorrect 0
       xlab = expression(theta - beta),
       ylab = "Proportion correct",
       ylim = c(0,1),
       xlim = c(-4,6.5),
       yaxt = 'n')
  
  # Plot only 0 and 1 on y-axis
  axis(2, 0:1, 0:1, las=1)
  
  # Add grid line
  lines(c(-4,6.5), rep(.5,2), col=rgb(.9,.9,.9, .6))
  
  # Create bin table
  correct.incorrect.per.bin <- table(data[ ,c('bins','correct_answered')])
  
  # Create proportion correct per bin
  proportion.correct.per.bin <- correct.incorrect.per.bin[,2] / ( correct.incorrect.per.bin[,1] + correct.incorrect.per.bin[,2] )
  
  # Add proportion correct line to plot
  lines(names(proportion.correct.per.bin),
        proportion.correct.per.bin, 
        col = "darkorange",
        lwd = 3)
  
  # Estimate a parameter with GLM
  # fit <-  glm(correct_answered ~ delta, data = data, family = binomial("logit"))
  fit <-  glm(correct_answered ~ 0 + delta, data = data, family = binomial(link='logit')) # offset to get only one parameter
  
  # Plot logistic curve
  curve(predict(fit, data.frame(delta=x), type="resp"),
        add = TRUE, 
        col = "darkgreen",
        lwd = 2)
  
  fit2 <-  glm(correct_answered ~ delta, data = data, family = binomial(link='logit')) 
  
  # Plot logistic curve
  curve(predict(fit2, data.frame(delta=x), type="resp"),
        add = TRUE,
        lty = 'dashed',
        col = "darkgreen",
        lwd = 2)
  
  # Evaluate fit.
  # summary(fit)
  
  # Add a pramater and n as text to plot
  text(-4.4,.9, paste("a =",round(fit2$coefficients["delta"],3)), pos=4)
  text(-4.4,.6, paste("n =",length(data$delta)), pos=4)
  text(-4.4,.1, paste("a =",round(fit$coefficients["delta"],3)), pos=4)
}

dev.off()

# Copy plot to manuscript
system("cp analyse_proportion_correct/Proportion_correct_2_models.pdf manuscript/images/")

# Delete all png files after stiched together in PHOTOSHOP
# path.to.folder  <- "analyse_proportion_correct/temp_proportio_correct_image_sequence/"
# files.to.delete <- list.files(path.to.folder)
# file.remove(paste(path.to.folder, files.to.delete, sep =''))