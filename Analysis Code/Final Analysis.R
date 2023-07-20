library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(coin)
library(PerformanceAnalytics)
library(Hmisc)

############################################
#            SET VARIABLES                 #
############################################
EXPORT_DIR <- "../Analysis Output/Final/"


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
merged_data.asd <- merged_data %>% dplyr::filter(Disorder == "ASD - Autism")
merged_data.nt <- merged_data %>% dplyr::filter(Disorder != "ASD - Autism")

############################################
#      COMPARE MEANS BETWEEN GROUPS        #
############################################

### GRAPHICAL COMPARISON

vars.mean_comparison <- list(
  nbm_brightness_ratio = "NBM Brightness Ratio",
  nbm_area_ratio = "NBM Area Ratio",
  vp_brightness = "VP Brightness",
  vp_area = "VP Area",
  oxtr_density_in_nbm = "OXTR Density in NBM",
  oxtr_density_in_vp = "OXTR Density in VP"
  )

for (variable_name in names(vars.mean_comparison)) {
  print(variable_name)
  variable_title <- vars.mean_comparison[[variable_name]]
  print(variable_title)
  
  name.plot <- paste(EXPORT_DIR, "asd_vs_nt_violin_", variable_name, ".png", sep = "")
  neurotype.levels <- as.factor(c("ASD - Autism", "Control"))
  
  vplot <- ggplot(merged_data, aes_string(x = "Disorder", y = variable_name, fill = "Disorder")) +
    geom_violin(width = .4, alpha = .5) +
    # ggplot2::scale_color_grey() +
    geom_boxplot(width = .1, cex = .5, color = "black") +
    ggplot2::scale_fill_grey() +
    theme_minimal() +
    scale_x_discrete(limits = neurotype.levels, labels = c("Autistic", "Allistic")) +
    xlab("Neurotype") +
    ylab(variable_title) +
    theme(legend.position = "none")
  
  ggsave(
    name.plot,
    vplot,
    width = 4,
    height = 4,
    dpi = 2000,
    bg = "white"
        )
}

### NUMERICAL COMPARISON
comparison_table <- data.frame(
  variable_names = character(),
  shapiro_wilk_asd = double(),
  shapiro_wilk_nt = double(),
  variance_f = double(),
  students_t = double(),
  welchs_t = double(),
  wilcoxon_mann_whitney = double()
  )

for (variable_name in names(vars.mean_comparison)) {
  print(variable_name)
  
  # shapiro-wilk test of normality
  test.sw.asd <- shapiro.test(merged_data.asd[,variable_name])
  test.sw.nt <- shapiro.test(merged_data.nt[,variable_name])
  print(test.sw.asd)
  print(test.sw.nt)
  
  # f-test to compare variances
  test.varf <- var.test(merged_data.asd[,variable_name], merged_data.nt[,variable_name])
  print(test.varf)
  
  # t-tests (Student's and Welch's), as well as Wilcoxon rank-sum test
  test.ts <- t.test(merged_data.asd[,variable_name], merged_data.nt[,variable_name], var.equal = T)
  test.tw <- t.test(merged_data.asd[,variable_name], merged_data.nt[,variable_name], var.equal = F)
  test.wmw <- coin::wilcox_test(reformulate('Disorder', variable_name), merged_data, distribution = "exact")
  print(test.ts)
  print(test.tw)
  print(test.wmw)
  
  comparison_table<-rbind(comparison_table,
        data.frame(
          variable_names = c(variable_name),
          shapiro_wilk_asd = c(test.sw.asd$p.value),
          shapiro_wilk_nt = c(test.sw.nt$p.value),
          variance_f = c(test.varf$p.value),
          students_t = c(test.ts$p.value),
          welchs_t = c(test.tw$p.value),
          wilcoxon_mann_whitney = c(pvalue(test.wmw))
        )
  )
  print("==========================================")
}

comparison_table


############################################
#      GENERATE CORRELATION TABLES         #
############################################

# generate sets of only continuous data
merged_data.coll.all <- data.frame(
  oxtr_density_in_nbm = merged_data$oxtr_density_in_nbm,
  oxtr_density_in_vp = merged_data$oxtr_density_in_vp,
  nbm_area_ratio = merged_data$nbm_area_ratio,
  nbm_brightness_ratio = merged_data$nbm_brightness_ratio,
  vp_area = merged_data$vp_area,
  vp_brightness = merged_data$vp_brightness,
  Age_yrs = merged_data$Age_yrs
)
merged_data.coll.asd <- data.frame(
  oxtr_density_in_nbm = merged_data.asd$oxtr_density_in_nbm,
  oxtr_density_in_vp = merged_data.asd$oxtr_density_in_vp,
  nbm_area_ratio = merged_data.asd$nbm_area_ratio,
  nbm_brightness_ratio = merged_data.asd$nbm_brightness_ratio,
  vp_area = merged_data.asd$vp_area,
  vp_brightness = merged_data.asd$vp_brightness,
  Age_yrs = merged_data.asd$Age_yrs
)
merged_data.coll.nt <- data.frame(
  oxtr_density_in_nbm = merged_data.nt$oxtr_density_in_nbm,
  oxtr_density_in_vp = merged_data.nt$oxtr_density_in_vp,
  nbm_area_ratio = merged_data.nt$nbm_area_ratio,
  nbm_brightness_ratio = merged_data.nt$nbm_brightness_ratio,
  vp_area = merged_data.nt$vp_area,
  vp_brightness = merged_data.nt$vp_brightness,
  Age_yrs = merged_data.nt$Age_yrs
)

### tables:
# generate matrices (all)
cortable.kt.all <- matrix(0, 4, 6, byrow = T)
colnames(cortable.kt.all) <- c("NBM OXTR Density τ", "NBM OXTR Density p-value", "NBM OXTR Density N", "VP OXTR Density τ", "VP OXTR Density p-value", "VP OXTR Density N")
rownames(cortable.kt.all) <- c("NBM Brightness Ratio", "NBM Area Ratio", "VP Brightness", "VP Area")

# get N (all)
merged_data.count.nod<-merged_data[!is.na(merged_data$oxtr_density_in_nbm),]
merged_data.count.vod<-merged_data[!is.na(merged_data$oxtr_density_in_vp),]

nod.nbr.all.n<-nrow(merged_data.count.nod[!is.na(merged_data.count.nod$nbm_brightness_ratio),])
nod.nar.all.n<-nod.nbr.all.n
nod.vb.all.n<-nrow(merged_data.count.nod[!is.na(merged_data.count.nod$vp_brightness),])
nod.va.all.n<-nod.vb.all.n

vod.nbr.all.n<-nrow(merged_data.count.vod[!is.na(merged_data.count.vod$nbm_brightness_ratio),])
vod.nar.all.n<-vod.nbr.all.n
vod.vb.all.n<-nrow(merged_data.count.vod[!is.na(merged_data.count.vod$vp_brightness),])
vod.va.all.n<-vod.vb.all.n

# run tests (all)
nod.nbr.all<-cor.test(merged_data$oxtr_density_in_nbm, merged_data$nbm_brightness_ratio, method = "kendall")
nod.nar.all<-cor.test(merged_data$oxtr_density_in_nbm, merged_data$nbm_area_ratio, method = "kendall")
nod.vb.all<-cor.test(merged_data$oxtr_density_in_nbm, merged_data$vp_brightness, method = "kendall")
nod.va.all<-cor.test(merged_data$oxtr_density_in_nbm, merged_data$vp_area, method = "kendall")
vod.nbr.all<-cor.test(merged_data$oxtr_density_in_vp, merged_data$nbm_brightness_ratio, method = "kendall")
vod.nar.all<-cor.test(merged_data$oxtr_density_in_vp, merged_data$nbm_area_ratio, method = "kendall")
vod.vb.all<-cor.test(merged_data$oxtr_density_in_vp, merged_data$vp_brightness, method = "kendall")
vod.va.all<-cor.test(merged_data$oxtr_density_in_vp, merged_data$vp_area, method = "kendall")

# fill in matrices (all)
cortable.kt.all[1,1] = nod.nbr.all$estimate
cortable.kt.all[1,2] = nod.nbr.all$p.value
cortable.kt.all[1,3] = nod.nbr.all.n
cortable.kt.all[1,4] = vod.nbr.all$estimate
cortable.kt.all[1,5] = vod.nbr.all$p.value
cortable.kt.all[1,6] = vod.nbr.all.n
cortable.kt.all[2,1] = nod.nar.all$estimate
cortable.kt.all[2,2] = nod.nar.all$p.value
cortable.kt.all[2,3] = nod.nar.all.n
cortable.kt.all[2,4] = vod.nar.all$estimate
cortable.kt.all[2,5] = vod.nar.all$p.value
cortable.kt.all[2,6] = vod.nar.all.n
cortable.kt.all[3,1] = nod.vb.all$estimate
cortable.kt.all[3,2] = nod.vb.all$p.value
cortable.kt.all[3,3] = nod.vb.all.n
cortable.kt.all[3,4] = vod.vb.all$estimate
cortable.kt.all[3,5] = vod.vb.all$p.value
cortable.kt.all[3,6] = vod.vb.all.n
cortable.kt.all[4,1] = nod.va.all$estimate
cortable.kt.all[4,2] = nod.va.all$p.value
cortable.kt.all[4,3] = nod.va.all.n
cortable.kt.all[4,4] = vod.va.all$estimate
cortable.kt.all[4,5] = vod.va.all$p.value
cortable.kt.all[4,6] = vod.va.all.n

# generate matrices (asd)
cortable.kt.asd <- matrix(0, 4, 6, byrow = T)
colnames(cortable.kt.asd) <- c("NBM OXTR Density τ", "NBM OXTR Density p-value", "NBM OXTR Density N", "VP OXTR Density τ", "VP OXTR Density p-value", "VP OXTR Density N")
rownames(cortable.kt.asd) <- c("NBM Brightness Ratio", "NBM Area Ratio", "VP Brightness", "VP Area")

# get N (asd)
merged_data.asd.count.nod<-merged_data.asd[!is.na(merged_data.asd$oxtr_density_in_nbm),]
merged_data.asd.count.vod<-merged_data.asd[!is.na(merged_data.asd$oxtr_density_in_vp),]

nod.nbr.asd.n<-nrow(merged_data.asd.count.nod[!is.na(merged_data.asd.count.nod$nbm_brightness_ratio),])
nod.nar.asd.n<-nod.nbr.asd.n
nod.vb.asd.n<-nrow(merged_data.asd.count.nod[!is.na(merged_data.asd.count.nod$vp_brightness),])
nod.va.asd.n<-nod.vb.asd.n

vod.nbr.asd.n<-nrow(merged_data.asd.count.vod[!is.na(merged_data.asd.count.vod$nbm_brightness_ratio),])
vod.nar.asd.n<-vod.nbr.asd.n
vod.vb.asd.n<-nrow(merged_data.asd.count.vod[!is.na(merged_data.asd.count.vod$vp_brightness),])
vod.va.asd.n<-vod.vb.asd.n

# run tests (asd)
nod.nbr.asd<-cor.test(merged_data.asd$oxtr_density_in_nbm, merged_data.asd$nbm_brightness_ratio, method = "kendall")
nod.nar.asd<-cor.test(merged_data.asd$oxtr_density_in_nbm, merged_data.asd$nbm_area_ratio, method = "kendall")
nod.vb.asd<-cor.test(merged_data.asd$oxtr_density_in_nbm, merged_data.asd$vp_brightness, method = "kendall")
nod.va.asd<-cor.test(merged_data.asd$oxtr_density_in_nbm, merged_data.asd$vp_area, method = "kendall")
vod.nbr.asd<-cor.test(merged_data.asd$oxtr_density_in_vp, merged_data.asd$nbm_brightness_ratio, method = "kendall")
vod.nar.asd<-cor.test(merged_data.asd$oxtr_density_in_vp, merged_data.asd$nbm_area_ratio, method = "kendall")
vod.vb.asd<-cor.test(merged_data.asd$oxtr_density_in_vp, merged_data.asd$vp_brightness, method = "kendall")
vod.va.asd<-cor.test(merged_data.asd$oxtr_density_in_vp, merged_data.asd$vp_area, method = "kendall")

# fill in matrices (asd)
cortable.kt.asd[1,1] = nod.nbr.asd$estimate
cortable.kt.asd[1,2] = nod.nbr.asd$p.value
cortable.kt.asd[1,3] = nod.nbr.asd.n
cortable.kt.asd[1,4] = vod.nbr.asd$estimate
cortable.kt.asd[1,5] = vod.nbr.asd$p.value
cortable.kt.asd[1,6] = vod.nbr.asd.n
cortable.kt.asd[2,1] = nod.nar.asd$estimate
cortable.kt.asd[2,2] = nod.nar.asd$p.value
cortable.kt.asd[2,3] = nod.nar.asd.n
cortable.kt.asd[2,4] = vod.nar.asd$estimate
cortable.kt.asd[2,5] = vod.nar.asd$p.value
cortable.kt.asd[2,6] = vod.nar.asd.n
cortable.kt.asd[3,1] = nod.vb.asd$estimate
cortable.kt.asd[3,2] = nod.vb.asd$p.value
cortable.kt.asd[3,3] = nod.vb.asd.n
cortable.kt.asd[3,4] = vod.vb.asd$estimate
cortable.kt.asd[3,5] = vod.vb.asd$p.value
cortable.kt.asd[3,6] = vod.vb.asd.n
cortable.kt.asd[4,1] = nod.va.asd$estimate
cortable.kt.asd[4,2] = nod.va.asd$p.value
cortable.kt.asd[4,3] = nod.va.asd.n
cortable.kt.asd[4,4] = vod.va.asd$estimate
cortable.kt.asd[4,5] = vod.va.asd$p.value
cortable.kt.asd[4,6] = vod.va.asd.n

# generate matrices (nt)
cortable.kt.nt <- matrix(0, 4, 6, byrow = T)
colnames(cortable.kt.nt) <- c("NBM OXTR Density τ", "NBM OXTR Density p-value", "NBM OXTR Density N", "VP OXTR Density τ", "VP OXTR Density p-value", "VP OXTR Density N")
rownames(cortable.kt.nt) <- c("NBM Brightness Ratio", "NBM Area Ratio", "VP Brightness", "VP Area")

# get N (nt)
merged_data.nt.count.nod<-merged_data.nt[!is.na(merged_data.nt$oxtr_density_in_nbm),]
merged_data.nt.count.vod<-merged_data.nt[!is.na(merged_data.nt$oxtr_density_in_vp),]

nod.nbr.nt.n<-nrow(merged_data.nt.count.nod[!is.na(merged_data.nt.count.nod$nbm_brightness_ratio),])
nod.nar.nt.n<-nod.nbr.nt.n
nod.vb.nt.n<-nrow(merged_data.nt.count.nod[!is.na(merged_data.nt.count.nod$vp_brightness),])
nod.va.nt.n<-nod.vb.nt.n

vod.nbr.nt.n<-nrow(merged_data.nt.count.vod[!is.na(merged_data.nt.count.vod$nbm_brightness_ratio),])
vod.nar.nt.n<-vod.nbr.nt.n
vod.vb.nt.n<-nrow(merged_data.nt.count.vod[!is.na(merged_data.nt.count.vod$vp_brightness),])
vod.va.nt.n<-vod.vb.nt.n

# run tests (nt)
nod.nbr.nt<-cor.test(merged_data.nt$oxtr_density_in_nbm, merged_data.nt$nbm_brightness_ratio, method = "kendall")
nod.nar.nt<-cor.test(merged_data.nt$oxtr_density_in_nbm, merged_data.nt$nbm_area_ratio, method = "kendall")
nod.vb.nt<-cor.test(merged_data.nt$oxtr_density_in_nbm, merged_data.nt$vp_brightness, method = "kendall")
nod.va.nt<-cor.test(merged_data.nt$oxtr_density_in_nbm, merged_data.nt$vp_area, method = "kendall")
vod.nbr.nt<-cor.test(merged_data.nt$oxtr_density_in_vp, merged_data.nt$nbm_brightness_ratio, method = "kendall")
vod.nar.nt<-cor.test(merged_data.nt$oxtr_density_in_vp, merged_data.nt$nbm_area_ratio, method = "kendall")
vod.vb.nt<-cor.test(merged_data.nt$oxtr_density_in_vp, merged_data.nt$vp_brightness, method = "kendall")
vod.va.nt<-cor.test(merged_data.nt$oxtr_density_in_vp, merged_data.nt$vp_area, method = "kendall")

# fill in matrices (nt)
cortable.kt.nt[1,1] = nod.nbr.nt$estimate
cortable.kt.nt[1,2] = nod.nbr.nt$p.value
cortable.kt.nt[1,3] = nod.nbr.nt.n
cortable.kt.nt[1,4] = vod.nbr.nt$estimate
cortable.kt.nt[1,5] = vod.nbr.nt$p.value
cortable.kt.nt[1,6] = vod.nbr.nt.n
cortable.kt.nt[2,1] = nod.nar.nt$estimate
cortable.kt.nt[2,2] = nod.nar.nt$p.value
cortable.kt.nt[2,3] = nod.nar.nt.n
cortable.kt.nt[2,4] = vod.nar.nt$estimate
cortable.kt.nt[2,5] = vod.nar.nt$p.value
cortable.kt.nt[2,6] = vod.nar.nt.n
cortable.kt.nt[3,1] = nod.vb.nt$estimate
cortable.kt.nt[3,2] = nod.vb.nt$p.value
cortable.kt.nt[3,3] = nod.vb.nt.n
cortable.kt.nt[3,4] = vod.vb.nt$estimate
cortable.kt.nt[3,5] = vod.vb.nt$p.value
cortable.kt.nt[3,6] = vod.vb.nt.n
cortable.kt.nt[4,1] = nod.va.nt$estimate
cortable.kt.nt[4,2] = nod.va.nt$p.value
cortable.kt.nt[4,3] = nod.va.nt.n
cortable.kt.nt[4,4] = vod.va.nt$estimate
cortable.kt.nt[4,5] = vod.va.nt$p.value
cortable.kt.nt[4,6] = vod.va.nt.n

# export matrices (all/asd/nt)
write.csv(cortable.kt.all, paste(EXPORT_DIR, "kendall_correlations_all.csv"))
write.csv(cortable.kt.asd, paste(EXPORT_DIR, "kendall_correlations_asd.csv"))
write.csv(cortable.kt.nt, paste(EXPORT_DIR, "kendall_correlations_nt.csv"))


### charts:
png(paste(EXPORT_DIR, "correlation_pearson_all.png", sep=""), height = 2000, width = 2000, res = 300)
chart.Correlation(merged_data.coll.all, pch = 19, method = "pearson")
mtext("Pearson Correlations - All Samples", side = 3, line = 3, font = 2, cex = 1.4)
dev.off()

png(paste(EXPORT_DIR, "correlation_spearman_all.png", sep=""), height = 2000, width = 2000, res = 300)
chart.Correlation(merged_data.coll.all, pch = 19, method = "spearman")
mtext("Spearman Correlations - All Samples", side = 3, line = 3, font = 2, cex = 1.4)
dev.off()

png(paste(EXPORT_DIR, "correlation_kendall_all.png", sep=""), height = 2000, width = 2000, res = 300)
chart.Correlation(merged_data.coll.all, pch = 19, method = "kendall")
mtext("Kendall Correlations - All Samples", side = 3, line = 3, font = 2, cex = 1.4)
dev.off()

png(paste(EXPORT_DIR, "correlation_pearson_asd.png", sep=""), height = 2000, width = 2000, res = 300)
chart.Correlation(merged_data.coll.asd, pch = 19, method = "pearson")
mtext("Pearson Correlations - ASD Samples", side = 3, line = 3, font = 2, cex = 1.4)
dev.off()

png(paste(EXPORT_DIR, "correlation_spearman_asd.png", sep=""), height = 2000, width = 2000, res = 300)
chart.Correlation(merged_data.coll.asd, pch = 19, method = "spearman")
mtext("Spearman Correlations - ASD Samples", side = 3, line = 3, font = 2, cex = 1.4)
dev.off()

png(paste(EXPORT_DIR, "correlation_kendall_asd.png", sep=""), height = 2000, width = 2000, res = 300)
chart.Correlation(merged_data.coll.asd, pch = 19, method = "kendall")
mtext("Kendall Correlations - ASD Samples", side = 3, line = 3, font = 2, cex = 1.4)
dev.off()

png(paste(EXPORT_DIR, "correlation_pearson_nt.png", sep=""), height = 2000, width = 2000, res = 300)
chart.Correlation(merged_data.coll.nt, pch = 19, method = "pearson")
mtext("Pearson Correlations - NT Samples", side = 3, line = 3, font = 2, cex = 1.4)
dev.off()

png(paste(EXPORT_DIR, "correlation_spearman_nt.png", sep=""), height = 2000, width = 2000, res = 300)
chart.Correlation(merged_data.coll.nt, pch = 19, method = "spearman")
mtext("Spearman Correlations - NT Samples", side = 3, line = 3, font = 2, cex = 1.4)
dev.off()

png(paste(EXPORT_DIR, "correlation_kendall_nt.png", sep=""), height = 2000, width = 2000, res = 300)
chart.Correlation(merged_data.coll.nt, pch = 19, method = "kendall")
mtext("Kendall Correlations - NT Samples", side = 3, line = 3, font = 2, cex = 1.4)
dev.off()

graphics.off()




