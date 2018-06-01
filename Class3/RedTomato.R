RedTomato = function(x, fName){
  
  # Create plots folder if it doesn't exist
  if (!("Plots" %in% dir())){
    dir.create(path = "Plots")
  }
  
  # Start graphic device
  png(filename=paste0("./Plots/", fName, ".png"),
      type="cairo", width=900, height=600)
  
  # Layout four graphs on the same graphic device
  graphics::layout(mat = matrix(c(1,2,3,4), nrow = 2, ncol = 2))
  
  # Set up margins
  par(mai = c(.5,.8,.5,.5), mgp = c(2,.7,0))
  
  # Variable list
  v = list()
  v$orig = x
  v$orig.skew = skew(v$orig)
  
  # Plot Original variable
  hist(v$orig, cex.main = .9,
       main = paste0("Skewness: ", as.character(round(v$orig.skew, 3))),
       xlab = "Original")
  box()
  
  # Calculate and plot inverse transformation
  v$inv = 1/v$orig
  v$inv.skew = skew(v$inv)
  hist(v$inv, cex.main = .9,
       main = paste0("Skewness: ", as.character(round(v$inv.skew, 3))),
       xlab = "Inverse")
  box()
  
  # Calculate and plot natural log transformation
  v$log = log(v$orig)
  v$log.skew = skew(v$log)
  hist(v$log, cex.main = .9,
       main = paste0("Skewness: ", as.character(round(v$log.skew, 3))),
       xlab = "Natural Log")
  box()
  
  # Calculate and plot square root transformation
  v$sqrt = sqrt(v$orig)
  v$sqrt.skew = skew(v$sqrt)
  hist(v$sqrt, cex.main = .9,
       main = paste0("Skewness: ", as.character(round(v$sqrt.skew, 3))),
       xlab = "Square Root")
  box()
  
  # Close graphic device
  dev.off()  
}
