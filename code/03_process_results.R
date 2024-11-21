# FIgure
res <- readRDS(file="output/draws/combined_model/res_brw_2025_2024-11-20.RDS")

# Diagnostics
check_hmc_diagnostics(res) # do not look all to good


# Load Forcasts
df <- readRDS("output/zweitstimme_output.RDS")


ordered_party <- names(sort(-apply(df$forecast,2,median)))


# Adjust Order to size of party


forecast <- df$forecast
forecast <- forecast[, ordered_party]

# Define names
party_names <- data.frame("full_name" = c("CDU/CSU", "SPD",  "Grüne", "FDP", "AfD" ,"Linke", "BSW", "Andere"),
                          "full_name_eng" = c("CDU/CSU", "SPD",  "Grüne", "FDP", "AfD" ,"Linke", "BSW", "Andere"),
                          "short_name" =  c("cdu","spd","gru","fdp","afd","lin","bsw","oth"))
party_colors <- c(
  "CDU/CSU" = "black",
  "SPD" = "red",
  "Grüne" = "green3",
  "FDP" = "gold",
  "AfD" = "blue",
  "Linke" = "purple",
  "BSW" = "darkorange"
)


# Create Forcast CI dataframe
df_forecast <- data.frame(y = apply(forecast, 2,  mean),
                          ci = t(apply(forecast, 2, function(x) quantile(x, c(1/12, 11/12)))),
                          ci95 = t(apply(forecast,2, function(x) quantile(x, c(0.025, 0.975)))))
df_forecast <- round(df_forecast*100, 1)
colnames(df_forecast) <- c("value", "low", "high", "low95", "high95")


df_forecast$name <- party_names$full_name[match(rownames(df_forecast),party_names$short_name)]
df_forecast$name_eng <- party_names$full_name_eng[match(rownames(df_forecast),party_names$short_name)]

rownames(df_forecast) <- party_names$full_name[match(rownames(df_forecast),party_names$short_name)]





# Drop Andere
df_forecast <- filter(df_forecast, name != "Andere")

df_forecast$color <- party_colors[df_forecast$name]

df_forecast$y <- df_forecast$value
df_forecast$x <- seq(0, 6, 1)


pdf("output/fig/figure_forcast.pdf", width = 12, height = 8)
par(mar=c(5,5,0,0)+.1)
x_val <- 1:7
plot(x = x_val,
     y = df_forecast$value, col = "white", 
     type = "n", bty = "n", 
     ylim = c(0,45), xlim = c(0,7.5),
     xlab = "",
     ylab = "",
     yaxt = "n",
     xaxt = "n",
     cex.axis = 1.2)
abline(h = c(10,20,30,40,50), lty = "dashed", col = "lightgrey")
abline(h = c(0), lty = "solid", col = "lightgrey")
abline(h = c(5,15,25,35,45), lty = "dashed", col = adjustcolor("lightgrey", alpha = 0.5))

#segments(x0 = c(1,2,3,4,5,6,7), y0 = 0, y1 = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5), lwd = 35, col = adjustcolor("grey", alpha = 0.3), lend = 1)
segments(x0 = x_val, y0 = df_forecast$low95, y1 = df_forecast$high95, 
         lwd = 25, col = adjustcolor(df_forecast$color, alpha = 0.6), lend = 1)
segments(x0 = x_val, y0 = df_forecast$low, y1 = df_forecast$high, 
         lwd = 25, col = adjustcolor(df_forecast$color, alpha = 0.99), lend = 1)

points(x = x_val,
       y = df_forecast$value, col = "white", lwd = 2)
text(y = df_forecast$value, x = x_val-0.35, labels = df_forecast$value, cex = 0.9,  col = adjustcolor("black", alpha = 0.7))
#text(y = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5), x = x_val+0.35, labels = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5.0), cex = 0.9, col = adjustcolor("black", alpha = 0.4))
axis(1, at = x_val, labels = df_forecast$name_eng, las = 1, tick = 0, cex.axis = 1.2 )
axis(2, at = c(5,10,20,30,40,50), labels = c(5,10,20,30,40,50), las = 1, tick = 0, cex.axis = 1.2)
mtext("Vote Share (%)", side=2, line=3, cex=1.2)
dev.off()

