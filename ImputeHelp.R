# Help Function for Imputing
IncMat <- function (Data, P){
  NRes <- nrow(Data)
  NVar <- ncol(Data)
  for (i in seq(NRes)){
    for (j in sample(seq(NVar), P, replace = FALSE)){
      Data[i,j] = NA    
    }
  }
  return(Data)
}

# Linear Model Equation
lm_eqn = function(m){
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq))                
}

# Creating the Plots
MPlot <- function (MFrame, i){
  m = lm(MImp ~ MFull, data=MFrame)
  p <- qplot(MFull, MImp, data = MFrame,
             main = paste("Fold", i))
  p <- p + geom_text(aes(x = 2.7, y = 2.7,
                         label = lm_eqn(m), 
                         hjust= 1.5, vjust = 1), parse = TRUE)
  p <- p + geom_abline(intercept = coef(m)[1], 
                       slope = coef(m)[2], col = "red")
  p <- p + geom_abline(intercept = 0, slope = 1, col = "blue")
}
