##############
#            #
#  Clean up  #
#            #
##############

rm(list = ls()) # remove all data in ENVIRONMENT


######################
#                    #
#  Install Packages  #
#                    #
######################

if(!require(MESS)){install.packages("MESS", dependencies = TRUE)
  library(MESS)
}
if(!require(tidyverse)){install.packages("tidyverse", dependencies = TRUE)
  library(tidyverse)
}
if(!require(ragg)){install.packages("ragg", dependencies = TRUE)
  library(ragg)
}
if(!require(pracma)){install.packages("pracma", dependencies = TRUE)
  library(pracma)
}
if(!require(Cairo)){install.packages("Cairo", dependencies = TRUE)
  library(Cairo)
}
if(!require(devtools)){install.packages("devtools", dependencies = TRUE)
  library(devtools)
}
if(!require(defShit)){install_github("deftic/defShit", force = TRUE)
  library(defShit)
}

######################
#                    #
#     Load Data      #
#                    #
######################

pc_wd <- "C:/Users/bachmannch/Desktop"
mac_wd <- "/Volumes/DEFUNI B/aa recentWork/R"

if(Sys.info()['sysname'] == "Windows") {
  setwd(pc_wd)
  print("Working directory set to Windows.")
} else if (Sys.info()['sysname'] == "Darwin") {
  setwd(mac_wd)
  print("Working directory set to Unix.")
} else {
  print("Error setting working directory. Check file destination!")
}

theme_set(theme_classic(12))

dataAll <- read.csv("Peak_Characteristics.csv",
                    header = TRUE,
                    encoding = "UTF-8",
                    sep = ";",
                    dec = ".",
                    stringsAsFactors = FALSE
)

data <- dataAll[, ]

results <- data.frame(matrix(ncol = 7, nrow = 0))

for (i in 2:ncol(data)) {  # cycles through columns and applies function to get baseline, peak, fwhm, hrt and area under the curve  
  z <- subset(data, select = c(1, i))
  genotype_z <- ifelse(grepl("C", names(z[2])), "Control",
                       ifelse(grepl("P", names(z[2])), "Patient", NA))
  agonist_z <- ifelse(grepl("mM", names(z[2])), "KCl",
                      ifelse(grepl("uM", names(z[2])), "4-CMC", NA))
  concentration_z <- ifelse(grepl("5mM", names(z[2])), "5 mM",
                            ifelse(grepl("10mM", names(z[2])), "10 mM",
                                   ifelse(grepl("20mM", names(z[2])), "20 mM",
                                          ifelse(grepl("40mM", names(z[2])), "40 mM",
                                                 ifelse(grepl("60mM", names(z[2])), "60 mM",
                                                        ifelse(grepl("100mM", names(z[2])), "100 mM", 
                                                               ifelse(grepl("75uM", names(z[2])), "75 µM", 
                                                                      ifelse(grepl("150uM", names(z[2])), "150 µM", 
                                                                             ifelse(grepl("300uM", names(z[2])), "300 µM", 
                                                                                    ifelse(grepl("450uM", names(z[2])), "450 µM", 
                                                                                           ifelse(grepl("600uM", names(z[2])), "600 µM", 
                                                                                                  ifelse(grepl("750uM", names(z[2])), "750 µM", 
                                                                                                         ifelse(grepl("900uM", names(z[2])), "900 µM", NA
                                                                                                         )
                                                                                                  )
                                                                                           )
                                                                                    )
                                                                             )
                                                                      )
                                                               )
                                                        )
                                                 )
                                          )
                                   )
                            )
  )
  
  names(z) <- c("time", "value")
  fwhm_z <- fwhm.fun(z)
  hrt_z <- hrt.fun(z)
  ttp_z <- ttp.fun(z)
  area_z <- area.fun(z)
  result <- data.frame(genotype = genotype_z, concentration = concentration_z, agonist = agonist_z, fwhm = fwhm_z, hrt = hrt_z, ttp = ttp_z, area = area_z)
  results <- rbind(results, result)
  dims <- peakDim.fun(z)
  names(dims) <- c("xmax", "ymax", "ybase", "ypeak", "halfMax", "xAHigh", "xALow", "yAHigh", "yALow", "xBHigh", "xBLow", "yBHigh", "yBHigh", "mA", "aA", "xA", "mB", "aB", "xB", "fwhm", "hrt", "ttp", "area")
  
  labelBL <- paste0("a) baseline: ", round(dims[3], 1))
  labelFWHM <- paste0("c) FWHM: ", round(result[4], 2))
  labelHRT <- paste0("d) HRT: ", round(result[5], 2))
  labelTTP <- paste0("b) TTP: ", round(result[6], 0))
  labelAUC <- paste0("AUC: ", round(result[7], 1))
  
  con <- (max(z$value) - min(z$value)) / 15
  
  g <- ggplot(data = z, aes()) +
    #  ggplot(data = z, aes()) +
    geom_line(aes(x = time, y = value), size = 0.5, color = "#241611") +
    geom_point(aes(x = time, y = value), size = 0.5) +
    # lines and dots  
    geom_point(aes(x = dims$xA, y = dims$halfMax), color = "darkorange") +
    geom_point(aes(x = dims$xB, y = dims$halfMax), color = "darkorange") +
    geom_point(aes(x = dims$xmax, y = dims$ymax, color = "darkorange")) +
    geom_segment(aes(x = dims$xA, y = dims$halfMax, xend = dims$xB, yend = dims$halfMax), lty = 2, color = "darkorange") +
    geom_segment(aes(x = dims$xmax, y = dims$ymax, xend = dims$xmax, yend = dims$halfMax), lty = 2, color = "darkorange") +
    annotate("rect", xmin = 29, xmax = 42, ymin = dims$ymax - 5 * con, ymax = dims$ymax + con, alpha = 0.4, fill = "lightgray") +
    # baseline
    annotate("rect", xmin = 2, xmax = 5, ymin = min(z$value[2:5]), ymax = max(z$value[2:5]), alpha = 0.4, fill = "gray") +
    annotate("segment", x = 2, xend = 5, y = dims$ybase, yend = dims$ybase, size = 1, alpha = 0.5, color = "darkorange") +
    annotate("text", x = 3.5, y = max(z$value[2:5]) + con, label = "a", cex = 4, fontface = "bold") +
    annotate("text", x = 30, y = dims$ymax, label = labelBL, cex = 3, hjust = 0, fontface = "bold") +
    # ttp
    annotate("segment", x = dims$xmax - dims$ttp, xend = dims$xmax, y = dims$ybase - 0.5 * con, yend = dims$ybase - 0.5 * con, size = 1, color = "darkorange") +
    annotate("text", x = dims$xmax - dims$ttp / 2, y = dims$ybase - 1.2 * con, label = "b", cex = 4, fontface = "bold") +
    annotate("text", x = 30, y = dims$ymax - con, label = labelTTP, cex = 3, hjust = 0, fontface = "bold") +
    # fwhm
    geom_segment(aes(x = dims$xA + 0.2, xend = dims$xB - 0.2, y = dims$halfMax - 0.5 * con, yend = dims$halfMax - 0.5 * con), lty = 1) +
    annotate("text", x = dims$xA + dims$fwhm / 2, y = dims$halfMax - con, label = "c", cex = 4, fontface = "bold") +
    annotate("text", x = 30, y = dims$ymax - 2 * con, label = labelFWHM, cex = 3, hjust = 0, fontface = "bold") +
    # hrt
    geom_segment(aes(x = dims$xmax + 0.2, xend = dims$xB - 0.2, y = dims$halfMax + 0.5 * con, yend = dims$halfMax + 0.5 * con), lty = 1) +
    annotate("text", x = dims$xB - dims$hrt / 2, y = dims$halfMax + con, label = "d", cex = 4, fontface = "bold") +
    annotate("text", x = 30, y = dims$ymax - 3 * con, label = labelHRT, cex = 3, hjust = 0, fontface = "bold") +
    # annotate(geom = "curve", x = 30, y = dims$ymax, xend = 3.5, yend = max(z$value[2:5]), curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
    # area
    geom_area(data = subset(z, time >= (dims$xmax - dims$ttp) & time < which.max(z$value <= dims$ybase & z$time > dims$xmax)), aes(x = time, y = value), fill = "#00998a", alpha = .3) +
    geom_segment(aes(x = dims$xmax - dims$ttp, xend = which.max(z$value <= dims$ybase & z$time > dims$xmax), y = dims$ybase, yend = dims$ybase), lty = 2, color = "gray", alpha = 0.5, size = 0.5) +
    annotate("text", x = 30, y = dims$ymax - 4 * con, label = labelAUC, cex = 3, hjust = 0, fontface = "bold", color = "#00998a") +
    # settings
    coord_cartesian(ylim = c(min(z$value) - 3 * con, max(z$value) + con)) +
    #labs(x = "") +
    theme(legend.position="none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
    # g
    # print(g) # print graph on screen
  
  # ggsave(g, filename = paste("myplot_", i, ".png", sep = ""), type = "cairo", width = 12, height = 8, units = "cm", dpi = 150, path = "/Volumes/DEFUNI B/aa recentWork/R/plots/")
  ggsave(g, filename = paste("myplot_", i, ".png", sep = ""), device = agg_png, width = 12, height = 8, units = "cm", res = 150, path = paste(pc_wd, "/plots/", sep = ""))
  # ggsave(g, filename = paste("myplot_", i, ".pdf", sep = ""), device = cairo_pdf, width = 12, height = 8, units = "cm", path = "/Volumes/DEFUNI B/aa recentWork/R/plots/")
  
  
}

write.table(results, file = "results.csv", dec = ".", sep = ";", col.names = NA) # write control results to csv file named results.csv






