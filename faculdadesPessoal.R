library(RColorBrewer)

# Data from http://www.vrea.usp.br/?q=node/8
y2009 <- c(93.72,96.03,94.85,92.14,59.30,61.85,62.45,56.73,114.70,114.04,116.53,114.22,81.80,84.09,84.58,79.52,95.92,96.72,
                  103.48,92.25,93.84,95.65,95.80,92.14,84.63,87.51,86.71,82.37,79.69,82.51,82.71,77.06,84.83,87.61,86.81,82.67,81.61,
                  81.40,80.64,82.13,76.39,78.65,78.18,74.55,80.27,80.88,80.29,79.98)
y2010 <- c(68.66,70.92,69.97,67.03,103.17,104.00,103.84,102.49,77.65,80.55,79.95,75.30,78.61,80.88,80.16,76.88,78.07,78.35,82.57,75.97,84.96,86.03,85.82,
           84.09,79.62,81.25,80.52,78.47,82.13,85.11,83.88,79.99,80.50,83.06,81.39,78.92,80.93,83.72,81.71,79.28,79.95,82.78,80.65,78.32,81.79,83.93,80.48,81.36)

y2011 <- c(69.76,73.56,70.40,67.72,82.36,88.09,85.30,78.40,76.96,81.65,79.88,73.50,79.47,84.07,82.15,76.15,76.88,80.45,82.46,72.78,90.21,90.90,90.08,89.95,88.74,
           90.23,89.11,87.88,86.43,89.46,88.34,84.18,85.05,86.72,84.79,84.39,81.21,83.44,81.36,80.10,81.03,83.15,80.72,80.19,100.67,88.60,85.13,113.08)

y2012 <- c(90.25, 83.21,82.68,96.83,88.50,90.29,88.43,87.70,94.50,94.79,93.00,95.03,86.45,86.88,84.63,87.04,85.87,85.02,86.96,85.80,97.29,97.10,95.44,98.20,95.72,
           92.50,92.00,98.85,95.20,92.51,92.83,97.50,92.19,88.68,88.37,95.49,90.95,87.38,86.66,94.49,91.37,86.98,86.55,95.51,99.89,83.67,86.97,113.09)

y2013 <- c(106.15,91.19,91.64,119.47,103.32,100.15,102.83,105.00,93.24,90.01,91.40,95.56,96.08,91.19,92.92,99.75,95.44,85.47,87.73,103.46,99.34,92.75,93.93,104.77,
           87.81,82.99,83.95,91.74,91.12,87.47,89.99,93.31,91.19,87.27,88.26,94.30,96.14,86.83,110.30,94.29,96.00,90.63,92.18,100.17,92.76,83.78,84.70,100.46)

y2014 <- c(105.14,96.01,97.57,112.70,104.01,99.93,103.18,106.27,94.51,90.94,92.74,96.95)

# All in one table
yearMatrixlist <- lapply(list(y2009, y2010, y2011, y2012, y2013, y2014), matrix, ncol = 4, byrow= TRUE)
mtxGastoPessoal <- do.call(rbind, yearMatrixlist)

# To insert the date as date, transform in a df
dfGastoPessoal <- as.data.frame(mtxGastoPessoal)
dfGastoPessoal$Data <- seq.Date(as.Date("2009-01-01"), length.out = nrow(dfGastoPessoal), by= "month")
colnames(dfGastoPessoal) <- c("total", "unesp", "unicamp", "usp", "data")

# Just to look better
dfGastoPessoal <- dfGastoPessoal[,rev(colnames(dfGastoPessoal))] 


# Chart variables
faculdades <- c("unicamp", "unesp", "usp")
paleta <- brewer.pal(12, "Paired")
coresFaculdades <- c(unicamp = paleta[3], unesp = paleta[1], usp= paleta[5])
midleYear <- dfGastoPessoal$data[grepl("06-01", dfGastoPessoal$data)]
years <- gsub("-.*", "", midleYear)
yLim <- c(50,120)
yYearPos <- 35
  

png(filename = "pessoalUniversidadesEstaduaisPaulista.png", width = 1762, height = 808, pointsize=20)
par(family = "Palatino")

# Empty plot
plot(x = dfGastoPessoal$data, y = seq(from = yLim[1], to = yLim[2], length.out = nrow(dfGastoPessoal)), type = "n", xlab = "", ylab = "",
     axes = FALSE, main = "Impacto da folha de pagamento nas Universidades Estaduais Paulistas")
lwd = 3
for(i in faculdades) lines(x=dfGastoPessoal$data, y=dfGastoPessoal[,i], col = coresFaculdades[i], lwd = lwd )
axis(1, at = dfGastoPessoal$data, labels = format(dfGastoPessoal$data, "%b"), lwd = 0.2, cex.axis = 0.5)
axis(2, at = round(seq(from = yLim[1], to= yLim[2], 10)), las = 1, lwd = 0.2, cex.axis = 1)
legend(x = max(dfGastoPessoal$data), y = min(yLim), xjust=1, yjust = 0, box.lwd = 0, lwd = lwd,
       legend=names(coresFaculdades), lty =1, col = coresFaculdades)
par(xpd = TRUE)
mtext(text = "% da folha de pagamento bruta sobre as liberações financeiras", side=2, line= 3)
text(x = midleYear, y = yYearPos, labels= years)
dev.off()

