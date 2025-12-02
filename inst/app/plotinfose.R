# === Custom Test Information & SE Plot ===
plot_info_se <- function(mod) {
  # Create a theta grid
  theta <- seq(-10, 10, length = 500)
  
  # Compute test information and SE
  info <- testinfo(mod, theta)
  SE <- 1 / sqrt(info)
  
  # Combine into a data frame
  tinfo <- data.frame(theta = theta, info = info, SE = SE)
  
  # Find points where Information ≈ 1
  tinfo$diff <- abs(tinfo$info - 1)
  
  # Left point (negative theta)
  idx_left <- which.min(tinfo$diff[tinfo$theta < 0])
  theta_left <- tinfo$theta[tinfo$theta < 0][idx_left]
  info_left <- tinfo$info[tinfo$theta < 0][idx_left]
  
  # Right point (positive theta)
  idx_right <- which.min(tinfo$diff[tinfo$theta > 0])
  theta_right <- tinfo$theta[tinfo$theta > 0][idx_right]
  info_right <- tinfo$info[tinfo$theta > 0][idx_right]
  
  # ===== Plot base =====
  plot(tinfo$theta, tinfo$info, type = "n",
       ylim = c(0, max(tinfo$info) + 0.08*max(tinfo$info) ),
       xlim = c(theta_left-1, theta_right+1 ),
       xlab = expression(theta),
       ylab = "Information / SE",
       main = "Test Information and Standard Error")
  
  # ===== Shaded regions =====
  # Red = less reliable, Blue = reliable region
  polygon(c(tinfo$theta[tinfo$theta <= theta_left],
            rev(tinfo$theta[tinfo$theta <= theta_left])),
          c(tinfo$info[tinfo$theta <= theta_left],
            rep(0, sum(tinfo$theta <= theta_left))),
          col = rgb(1, 0, 0, 0.2), border = NA)
  
  polygon(c(tinfo$theta[tinfo$theta >= theta_right],
            rev(tinfo$theta[tinfo$theta >= theta_right])),
          c(tinfo$info[tinfo$theta >= theta_right],
            rep(0, sum(tinfo$theta >= theta_right))),
          col = rgb(1, 0, 0, 0.2), border = NA)
  
  polygon(c(tinfo$theta[tinfo$theta >= theta_left & tinfo$theta <= theta_right],
            rev(tinfo$theta[tinfo$theta >= theta_left & tinfo$theta <= theta_right])),
          c(tinfo$info[tinfo$theta >= theta_left & tinfo$theta <= theta_right],
            rep(0, sum(tinfo$theta >= theta_left & tinfo$theta <= theta_right))),
          col = rgb(0, 0, 1, 0.2), border = NA)
  
  # ===== Lines =====
  lines(tinfo$theta, tinfo$info, lwd = 2, col = "blue")
  lines(tinfo$theta, tinfo$SE, lwd = 2, col = "red", lty = 2)
  
  # ===== Reference lines and markers =====
  abline(h = 1, lty = 3, col = 'gray')
  abline(v = theta_left, lty = 2, col = 'gray')
  abline(v = theta_right, lty = 2, col = 'gray')
  
  points(theta_left, info_left, col = 'black', pch = 19, cex = 1.5)
  points(theta_right, info_right, col = 'black', pch = 19, cex = 1.5)
  
  text(theta_left, 1.7*info_left,
       labels = bquote(theta == .(round(theta_left, 2))),
       col = "black", pos = 2)
  
  text(theta_right, 1.7*info_right,
       labels = bquote(theta == .(round(theta_right, 2))),
       col = "black", pos = 4)
  
  # ===== Legend =====
  legend("topright", 
         legend = c("Test Information", "Standard Error", 
                    "Reliable Region", "Less Reliable Region"),
         col = c("blue", "red", rgb(0, 0, 1, 0.2), rgb(1, 0, 0, 0.2)),
         lty = c(1, 2, NA, NA), lwd = c(2, 2, NA, NA),
         pch = c(NA, NA, 15, 15), pt.cex = 2, bty = "n")
}
