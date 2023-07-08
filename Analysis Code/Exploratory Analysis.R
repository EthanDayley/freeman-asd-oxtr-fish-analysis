library(tidyverse)
library(broom)
library(ggfortify)
library(cowplot)


############################################
#            SET VARIABLES                 #
############################################
EXPORT_DIR <- "../Analysis Output/"


############################################
#        IMPORT AND PROCESS DATA           #
############################################

fish_data <- read.csv("../Preprocessed Statistics Files/nbm_and_vp_stats_merged.csv")
specimen_info <- read.csv("../Preprocessed Statistics Files/specimen info_ASD genetics.csv")

# change the first column name of specimen info, and perform a left join.
colnames(specimen_info)[1] = "sample_id"
merged_data <- merge(x = fish_data, y = specimen_info, by = "sample_id", all.x = T)

# remove samples without any corresponding binding data
merged_data<-merged_data[!is.na(merged_data$oxtr_density_in_nbm) | !is.na(merged_data$oxtr_density_in_vp),]

# change Disorder / Sex / Race to factors
merged_data$Disorder <- as.factor(merged_data$Disorder)
merged_data$Race <- as.factor(merged_data$Race)
merged_data$Sex <- as.factor(merged_data$Sex)

# generate split datasets from the merged data.
merged_data.asd <- merged_data %>% filter(Disorder == "ASD - Autism")
merged_data.nt <- merged_data %>% filter(Disorder != "ASD - Autism")

hist_vars <- c("nbm_brightness", "nbm_area", "nbm_area_ratio", "vp_brightness", "vp_area")

# generate log and sqrt transformations
for (variable_name in hist_vars) {
  merged_data[,paste(variable_name, ".log", sep="")] = log(merged_data[,variable_name])
  merged_data[,paste(variable_name, ".sqrt", sep="")] = sqrt(merged_data[,variable_name])
  merged_data.asd[,paste(variable_name, ".log", sep="")] = log(merged_data.asd[,variable_name])
  merged_data.asd[,paste(variable_name, ".sqrt", sep="")] = sqrt(merged_data.asd[,variable_name])
  merged_data.nt[,paste(variable_name, ".log", sep="")] = log(merged_data.nt[,variable_name])
  merged_data.nt[,paste(variable_name, ".sqrt", sep="")] = sqrt(merged_data.nt[,variable_name])
}

############################################
#        DISTRIBUTION STATS                #
############################################

# generate statistics and plots
dflist <- list(all = merged_data, asd = merged_data.asd, nt = merged_data.nt)


for (df.name in names(dflist)) {
  print(df.name)
  df<-dflist[[df.name]]
  
  for (variable_name in hist_vars) {
    plist <- list()
    
    print("############################")
    print(variable_name)
    
    # generate mean / sd /range
    v.mean<-mean(df[,variable_name], na.rm = T)
    v.sd<-sd(df[,variable_name], na.rm = T)
    v.range<-range(df[,variable_name], na.rm = T)
    v.mean.log<-mean(df[,paste(variable_name, ".log", sep="")], na.rm = T)
    v.sd.log<-sd(df[,paste(variable_name, ".log", sep="")], na.rm = T)
    v.range.log<-range(df[,paste(variable_name, ".log", sep="")], na.rm = T)
    v.mean.sqrt<-mean(df[,paste(variable_name, ".sqrt", sep="")], na.rm = T)
    v.sd.sqrt<-sd(df[,paste(variable_name, ".sqrt", sep="")], na.rm = T)
    v.range.sqrt<-range(df[,paste(variable_name, ".sqrt", sep="")], na.rm = T)
    
    # print mean / sd / range
    print("Non-transformed:")
    print(v.mean)
    print(v.sd)
    print(v.range)
    print("Log-transformed: ")
    print(v.mean.log)
    print(v.sd.log)
    print(v.range.log)
    print("Sqrt-transformed: ")
    print(v.mean.sqrt)
    print(v.sd.sqrt)
    print(v.range.sqrt)
    
    # generate normal curves
    norm_curve <- stat_function(fun = dnorm, args = list(mean = v.mean, sd = v.sd), col = "#1b98e0", size = 1)
    norm_curve.log <- stat_function(fun = dnorm, args = list(mean = v.mean.log, sd = v.sd.log), col = "#1b98e0", size = 1)
    norm_curve.sqrt <- stat_function(fun = dnorm, args = list(mean = v.mean.sqrt, sd = v.sd.sqrt), col = "#1b98e0", size = 1)
    
    # generate bin width
    bw <- 2 * IQR(df[,variable_name], na.rm=T) / length(df[,variable_name])^(1/3)
    bw.log <- 2 * IQR(df[,paste(variable_name, ".log", sep="")], na.rm=T) / length(df[,variable_name])^(1/3)
    bw.sqrt <- 2 * IQR(df[,paste(variable_name, ".sqrt", sep="")], na.rm=T) / length(df[,variable_name])^(1/3)
    
    # generate plots
    plot.hist <- ggplot(df, aes_string(x = variable_name)) +
      geom_histogram(aes(y = ..density..), color = "black", fill = "white", binwidth = bw) +
      geom_density(alpha = .2, fill = "#FF6666") +
      geom_vline(aes(xintercept = v.mean), color = "blue", linetype = "dashed", size = 1) +
      norm_curve
    plot.hist.log <- ggplot(df, aes_string(x = paste(variable_name, ".log", sep=""))) +
      geom_histogram(aes(y = ..density..), color = "black", fill = "white", binwidth = bw.log) +
      geom_density(alpha = .2, fill = "#FF6666") +
      geom_vline(aes(xintercept = v.mean.log), color = "blue", linetype = "dashed", size = 1) +
      norm_curve.log
    plot.hist.sqrt <- ggplot(df, aes_string(x = paste(variable_name, ".sqrt", sep=""))) +
      geom_histogram(aes(y = ..density..), color = "black", fill = "white", binwidth = bw.sqrt) +
      geom_density(alpha = .2, fill = "#FF6666") +
      geom_vline(aes(xintercept = v.mean.sqrt), color = "blue", linetype = "dashed", size = 1) +
      norm_curve.sqrt
    plot.hist.col<-ggplot(df, aes_string(x = variable_name, color = "Disorder", fill = "Disorder")) +
      scale_fill_manual(values = c("ASD - Autism" = "#619CFF", "Control" = "#F8766D")) +
      scale_color_manual(values = c("ASD - Autism" = "#619CFF", "Control" = "#F8766D")) +
      geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity", binwidth = bw)
    
    # add plots to list
    plist[[paste(variable_name, "plot.hist", sep=".")]] <- plot.hist
    plist[[paste(variable_name, "plot.hist.log", sep=".")]] <- plot.hist.log
    plist[[paste(variable_name, "plot.hist.sqrt", sep=".")]] <- plot.hist.sqrt
    plist[[paste(variable_name, "plot.hist.col", sep=".")]] <- plot.hist.col
    
    print("Outputting plots...")
    png(paste(EXPORT_DIR, "distribution_histograms_", df.name, "_", variable_name, ".png", sep=""), height = 4000, width = 4000, res = 300)
    plot.merged<-cowplot::plot_grid(plotlist = plist, labels = "AUTO", ncol = 2)
    print(plot.merged)
    dev.off()
  }
  print("============================================")
}

