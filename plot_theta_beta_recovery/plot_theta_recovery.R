# -----------------------------------------------------------------------------
# Create comparison plots for theta estimatons
# -----------------------------------------------------------------------------

# Clear memory
rm(list = ls())

# a.estimation.methods = c('LTM', 'GLM -int', 'GLM +int', 'NR')

type.name <- c('LTM Full', 'LTM Sparse', 'Elo Sparse')

# Set plot file location
pdf("plot_theta_beta_recovery/plot_theta_recovery.pdf", 
    height = 9, 
    width  = 8)

# Set margin and padding for multiple plots
par(oma = c(3, 2.5, 3, 0))
par(mar = c(2, 2,   2, 2))

# Set layout for plots
layout(matrix(1:9, 3,3,byrow=T), c(1,1,1), c(1,1,1), T)


# -----------------------------------------------------------------------------
# Scenario a = 1
# -----------------------------------------------------------------------------

# Load data
load("a-parameter_one_at_75/7_merge_estimations/all_theta_estimations.Rdata")

col.selection = grep('^theta|^rating\\.', names(all.theta.estimations), value = T)
col.selection = col.selection[c(2,3,1)] # Change to corresponding order

for(column in col.selection) {

  method <- type.name[ col.selection == column ]
    
  # par(mar = c(5, 2, 2, 2))
  
  plot(all.theta.estimations[,column],
       all.theta.estimations$user_rating,
       ylim = c(-3, 3),
       xlim = c(-3, 3),
       yaxt = 'n',
       ylab = "",
       xlab = NA,
       pch  = 16,
       col  = rgb(1, 0.5490196, 0, .5),
       # col  = ifelse(all.theta.estimations$nr.elo.one.counter > 2000, rgb(0, 0.3921569, 0, .5),
       #                                                          rgb(1, 0.5490196, 0, .5)),
       main = method
       )
    
  abline(coef = c(0,1), col = rgb(.75, .75, .75, .5))
  
  title(ylab=expression(theta), line=0, cex.lab=1.2)
  
  if(column == col.selection[1]) mtext("a = 1", outer = F, side = 2, padj = -3.5, cex = .6)
}

# all.theta.estimations$nr.elo.n[is.na(all.theta.estimations$nr.elo.n)] = 0
# gradient <- all.theta.estimations$nr.elo.n/max(all.theta.estimations$nr.elo.n, na.rm = T)

# -----------------------------------------------------------------------------
# Scenario a = [0, 2.5]
# -----------------------------------------------------------------------------

# Load data
load("a-parameter_negative_a/7_merge_estimations/all_theta_estimations.Rdata")

col.selection = grep('^theta|^rating\\.', names(all.theta.estimations), value = T)
col.selection = col.selection[c(2,3,1)] # Change to corresponding order

for(column in col.selection) {

  plot(all.theta.estimations[,column],
       all.theta.estimations$user_rating,
       ylim = c(-3, 3),
       xlim = c(-3, 3),
       yaxt = 'n',
       ylab = expression(theta),
       xlab = NA,
       pch  = 16,
       col  = rgb(1, 0.5490196, 0, .5),
       # col  = ifelse(all.theta.estimations$nr.elo.neg.counter > 2000, rgb(0, 0.3921569, 0, .5),
       #                                                rgb(1, 0.5490196, 0, .5)),
       asp=1
       )
  
  abline(coef = c(0,1), col = rgb(.75, .75, .75, .5))
  
  title(ylab=expression(theta), line=0, cex.lab=1.2)
  
  if(column == col.selection[1]) mtext("a ~U(0, 2.5)", outer = F, side = 2, padj = -3.5, cex = .6)
}

# -----------------------------------------------------------------------------
# Scenario a = [0, 1]
# -----------------------------------------------------------------------------

# Load data
load("a-parameter_zero_or_one/7_merge_estimations/all_theta_estimations.Rdata")

col.selection = grep('^theta|^rating\\.', names(all.theta.estimations), value = T)
col.selection = col.selection[c(2,3,1)] # Change to corresponding order

# par(oma = c(0, 2.5, 3, 0))
# par(mar = c(3, 2,   2, 2))

for(column in col.selection) {

  plot(all.theta.estimations[,column],
       all.theta.estimations$user_rating,
       ylim = c(-3, 3),
       xlim = c(-3, 3),
       yaxt = 'n',
       ylab = expression(theta),
       # xlab = expression(paste("Recovered ",a)),
       xlab = expression(hat(theta)),
       pch  = 16,
       col  = rgb(1, 0.5490196, 0, .5)
       )
  
  abline(coef = c(0,1), col = rgb(.75, .75, .75, .5))
  
  title(ylab=expression(theta), line=0, cex.lab=1.2)
  
  if(column == col.selection[1]) mtext("a = [.1, 1]", outer = F, side = 2, padj = -3.5, cex = .6)
}

tittle <- expression(paste("Recovery of ", theta))

mtext(tittle, outer = TRUE)

mtext(expression(hat(theta)), outer = TRUE, side = 1)

dev.off()

# Copy plot to manuscript
# system("cp plot_a_parameter_recovery_comparison/a_parameter_recovery_comparison_2par.pdf manuscript/images/")


