# -----------------------------------------------------------------------------
# Create comparison plots for beta estimatons
# -----------------------------------------------------------------------------

# Clear memory
rm(list = ls())

a.estimation.methods = c('LTM', 'GLM -int', 'GLM +int', 'NR')

type.name <- rbind(c('elo', 'full', 'sparse'),
                   c('Elo Sparse', 'LTM Full', ' LTM Sparse'))

for(i in 1:3) {

type <- type.name[,i]
  
# Set plot file location
pdf(paste0("plot_a_parameter_recovery_comparison/a_parameter_recovery_comparison_",type,".pdf"), 
    height = 5, 
    width  = 8.5)

# Set margin and padding for multiple plots
par(oma = c(0, 2.5, 3, 0))
par(mar = c(2, 2,   2, 2))

# Set layout for plots
layout(matrix(1:12, 3,4,byrow=T), c(1,1,1,1), c(.5,.9,.65), T)


# -----------------------------------------------------------------------------
# Scenario a = 1
# -----------------------------------------------------------------------------

# Load data
load("a-parameter_one_at_75/7_merge_estimations/all_estimations.Rdata")

col.selection = grep(paste0(type[1],'.*\\.a$'), names(all.estimations), value = T)
# col.selection = col.selection[-3]

for(column in col.selection) {

  method <- a.estimation.methods[ col.selection == column ]  
    
  # par(mar = c(5, 2, 2, 2))
  
  plot(all.estimations[,column],
       all.estimations$item_a_par,
       ylim = c(.9, 1.1),
       xlim = c(-.2, 1.5),
       yaxt = 'n',
       ylab = "",
       xlab = NA,
       pch  = 16,
       col  = rgb(0, 0.3921569,0, .5),
       # col  = ifelse(all.estimations$nr.elo.one.counter > 2000, rgb(0, 0.3921569, 0, .5),
       #                                                          rgb(1, 0.5490196, 0, .5)),
       main = method)
  
  abline(1,0, col = rgb(.75, .75, .75, .5))
  lines(c(1,1), c(-4,4), col = rgb(.75, .75, .75, .5))
  
  if(column == col.selection[1]) mtext("a = 1", outer = F, side = 2, padj = -3.5, cex = .6)
}

# all.estimations$nr.elo.n[is.na(all.estimations$nr.elo.n)] = 0
# gradient <- all.estimations$nr.elo.n/max(all.estimations$nr.elo.n, na.rm = T)

# -----------------------------------------------------------------------------
# Scenario a = [-.5, 2.5]
# -----------------------------------------------------------------------------

# Load data
load("a-parameter_negative_a/7_merge_estimations/all_estimations.Rdata")

col.selection = grep(paste0(type[1],'.*\\.a$'), names(all.estimations), value = T)
# col.selection = col.selection[-3]

for(column in col.selection) {

  plot(all.estimations[,column],
       all.estimations$item_a_par,
       ylim = c(-1, 3),
       xlim = c(-1, 3),
       yaxt = 'n',
       ylab = "",
       xlab = NA,
       pch  = 16,
       col  = rgb(0, 0.3921569,0, .5),
       # col  = ifelse(all.estimations$nr.elo.neg.counter > 2000, rgb(0, 0.3921569, 0, .5),
       #                                                rgb(1, 0.5490196, 0, .5)),
       asp=1 
       )
  
  abline(0,1, col = rgb(.75, .75, .75, .5))
  
  # all.estimations$nr.elo.n[is.na(all.estimations$nr.elo.n)] = 0
  # gradient <- all.estimations$nr.elo.n/max(all.estimations$nr.elo.n, na.rm = T)
  
  lines(c(.5,.5), c(-4,4), lty = 2, col = rgb(.75, .75, .75, .75))
  text(.5, -1, "cut-off", 
       pos    = 4, 
       srt    = "90",
       offset = -.5,
       col    = rgb(.75, .75, .75, 1))
  
  if(column == col.selection[1]) mtext("a ~U(0, 2.5)", outer = F, side = 2, padj = -3.5, cex = .6)
}

# -----------------------------------------------------------------------------
# Scenario a = [0, 1]
# -----------------------------------------------------------------------------

# Load data
load("a-parameter_zero_or_one/7_merge_estimations/all_estimations.Rdata")

col.selection = grep(paste0(type[1],'.*\\.a$'), names(all.estimations), value = T)
# col.selection = col.selection[-3]

par(mar = c(5, 2, 2, 2))

for(column in col.selection) {

  plot(all.estimations[,column],
       all.estimations$item_a_par,
       ylim = c(-.2, 1.3),
       xlim = c(-.1, 1.5),
       yaxt = 'n',
       ylab = "",
       xlab = expression(hat(a)),
       pch  = 16,
       col  = rgb(0, 0.3921569,0, .5)
       )
  
  abline(0.1,0, col = rgb(.75, .75, .75, .5))
  abline(1.0,0, col = rgb(.75, .75, .75, .5))
  lines(c(0.1,0.1), c(-4,4), col = rgb(.75, .75, .75, .5))
  lines(c(1  ,1  ), c(-4,4), col = rgb(.75, .75, .75, .5))
  
  lines(c(.5,.5), c(-4,4), lty = 2, col = rgb(.75, .75, .75, .75))
  
  if(column == col.selection[1]) mtext("a = [.1, 1]", outer = F, side = 2, padj = -3.5, cex = .6)
}

# legend("bottomright", c(expression("n"<="2000"), expression("n">"2000")), bty = "n", pch = 16, col = c(rgb(1, 0.5490196,0, 1), rgb(0, 0.3921569,0, 1)))

tittle <- paste(type[2], "Type Simulation")

mtext(tittle, outer = TRUE)

dev.off()

}

# Copy plot to manuscript
# system("cp plot_a_parameter_recovery_comparison/a_parameter_recovery_comparison_2par.pdf manuscript/images/")


