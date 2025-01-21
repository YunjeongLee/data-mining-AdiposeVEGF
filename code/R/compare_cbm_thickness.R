# Clear all ---------------------------------------------------------------
# Clear plots
if (!is.null(dev.list())) dev.off()
# Clear console
cat("\014")
# Clear workspace
rm(list = ls())

# Change working directory ------------------------------------------------
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Add path ----------------------------------------------------------------
subfolders = c("etc", "visualize", "analysis")
for (i in 1:length(subfolders)) {
  a = list.files(path = subfolders[i], pattern = "[.]R$", full.names = TRUE)
  for (j in 1:length(a)) {
    source(a[j])
  }
}

# Check and generate a result folder --------------------------------------
results_path = "../../results/figures/R"

# Generate new results folder
dir.create(results_path, recursive = TRUE)

# Load libraries ----------------------------------------------------------
pkg_list = c("ggplot2", "metafor", "readxl", "weights", "latex2exp", "ggpubr", 
             "shades", "ggnewscale", "scales", "ggsignif", "colormap", "stringr", 
             "dplyr", "pals", "PMCMRplus", "pheatmap", "magick")
instant_pkgs(pkg_list)

# Load data ---------------------------------------------------------------
filename = '../../data/adipose_tissue_parameters.xlsx'
cbm <- as.data.frame(read_excel(filename, sheet = "CBM thickness"))

# Split dataframe into small dataframes -----------------------------------
cbm_lean = cbm[!is.na(cbm["Lean SE"]), c("Reference", "Lean average", "Lean SE")]
cbm_obese = cbm[!is.na(cbm["Obese SE"]), c("Reference", "Obese average", "Obese SE")]
cbm_obese$Reference <- str_replace(cbm_obese$Reference, "LETO", "OLETF")

# Change column names -----------------------------------------------------
colnames(cbm_lean) <- c("Reference", "Average", "SE")
colnames(cbm_obese) <- c("Reference", "Average", "SE")

# Split CBM thickness data of lean murines into tissue-dependent --------
cbm_retina = cbm_lean %>% filter(str_detect(Reference, "retina"))
cbm_muscle = cbm_lean %>% filter(str_detect(Reference, "muscle"))
cbm_heart = cbm_lean %>% filter(str_detect(Reference, "heart"))
cbm_brain = cbm_lean %>% filter(str_detect(Reference, "brain"))
cbm_kidney = cbm_lean %>% filter(str_detect(Reference, "kidney"))

# Merge retina, muscle, and heart CBM thickness to generate lean mice data
cbm_lean_tis = cbm_lean %>% filter(str_detect(Reference, "retina|muscle|heart"))

# Remove substring from tissue dataframes
cbm_retina$Reference <- str_remove(cbm_retina$Reference, ", retina")
cbm_muscle$Reference <- str_remove(cbm_muscle$Reference, ", muscle")
cbm_heart$Reference <- str_remove(cbm_heart$Reference, ", heart")
cbm_brain$Reference <- str_remove(cbm_brain$Reference, ", brain")
cbm_kidney$Reference <- str_remove(cbm_kidney$Reference, ", kidney")

# Data-analysis -----------------------------------------------------------
# Capillary BM thickness of lean mice including only retina, muscle, and heart data
rm_cbm_lean_tis <- rma(yi = Average, sei = SE, data=cbm_lean_tis)
summary(rm_cbm_lean_tis)

# Capillary BM thickness of obese mice
rm_cbm_obese <- rma(yi = Average, sei = SE, data=cbm_obese)
summary(rm_cbm_obese)

# Tissue variability
# Retina (without obese data)
rm_cbm_retina <- rma(yi = Average, sei = SE, data=cbm_retina)
summary(rm_cbm_retina)

# Muscle (without obese data)
rm_cbm_muscle <- rma(yi = Average, sei = SE, data=cbm_muscle)
summary(rm_cbm_muscle)

# Heart (without obese data)
rm_cbm_heart <- rma(yi = Average, sei = SE, data=cbm_heart)
summary(rm_cbm_heart)

# Brain (without obese data)
rm_cbm_brain <- rma(yi = Average, sei = SE, data=cbm_brain)
summary(rm_cbm_brain)

# Kidney (without obese data)
rm_cbm_kidney <- rma(yi = Average, sei = SE, data=cbm_kidney)
summary(rm_cbm_kidney)

# Forest plot -------------------------------------------------------------
# CBM thickness of lean mice
png(file=sprintf("%s/forest_cbm_lean.png", results_path), width=2000, height=3000)
forest_ylee(data=cbm_lean_tis, rm=rm_cbm_lean_tis, slab=cbm_lean_tis$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-350, 450), alim = c(0, 250), cex=2, numDigits=0L)
dev.off()
save_as_pdf(sprintf("%s/forest_cbm_lean", results_path), width=7)

# CBM thickness of obese mice
png(file=sprintf("%s/forest_cbm_obese.png", results_path), width=1300, height=700)
forest_ylee(data=cbm_obese, rm=rm_cbm_obese, slab=cbm_obese$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-300, 400), alim = c(0, 200), cex=2, numDigits=0L)
dev.off()
save_as_pdf(sprintf("%s/forest_cbm_obese", results_path), width=4)

# Tissue variability
# Retina
png(file=sprintf("%s/forest_cbm_retina.png", results_path), width=1300, height=1700)
forest_ylee(data=cbm_retina, rm=rm_cbm_retina, slab=cbm_retina$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-750, 650), alim = c(0, 300), cex=2, numDigits=0L)
dev.off()
save_as_pdf(sprintf("%s/forest_cbm_retina", results_path), width=4)

# Muscle
png(file=sprintf("%s/forest_cbm_muscle.png", results_path), width=1300, height=600)
forest_ylee(data=cbm_muscle, rm=rm_cbm_muscle, slab=cbm_muscle$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-300, 300), alim = c(0, 150), cex=2, numDigits=0L)
dev.off()
save_as_pdf(sprintf("%s/forest_cbm_muscle", results_path), width=4)

# Heart
png(file=sprintf("%s/forest_cbm_heart.png", results_path), width=1300, height=700)
forest_ylee(data=cbm_heart, rm=rm_cbm_heart, slab=cbm_heart$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-150, 220), alim = c(0, 120), cex=2, numDigits=0L)
dev.off()
save_as_pdf(sprintf("%s/forest_cbm_heart", results_path), width=4)

# Brain
png(file=sprintf("%s/forest_cbm_brain.png", results_path), width=1300, height=1000)
forest_ylee(data=cbm_brain, rm=rm_cbm_brain, slab=cbm_brain$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-250, 250), alim = c(0, 150), cex=2, numDigits=0L)
dev.off()
save_as_pdf(sprintf("%s/forest_cbm_brain", results_path), width=4)

# Kidney
png(file=sprintf("%s/forest_cbm_kidney.png", results_path), width=1300, height=1000)
forest_ylee(data=cbm_kidney, rm=rm_cbm_kidney, slab=cbm_kidney$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-500, 700), alim = c(0, 400), cex=2, numDigits=0L)
dev.off()
save_as_pdf(sprintf("%s/forest_cbm_kidney", results_path), width=4)

# Student's t-test --------------------------------------------------------
# Lean vs. Obese (without kidney data)
cbm_lean_vs_obese = wtd.t.test(x=cbm_lean_tis$Average, y=cbm_obese$Average,
                               weight=1/(cbm_lean_tis$SE^2+rm_cbm_lean_tis$tau2), 
                               weighty=1/(cbm_obese$SE^2+rm_cbm_obese$tau2),
                               alternative="two.tailed", samedata=FALSE)

# Perform ANOVA + Dunnett's T3 test ---------------------------------------
rma_list = list(rm_cbm_retina, rm_cbm_muscle, rm_cbm_heart, rm_cbm_brain, rm_cbm_kidney)
tissue_list = c("Retina", "Muscle", "Heart", "Brain", "Kidney")
filename = sprintf('%s/cbm_posthoc.png', results_path)
result = anova_posthoc(rma_list = rma_list, tissue_list = tissue_list, filename=filename)

# Merge dataframes for plotting -------------------------------------------
cbm_retina$Tissue <- "Retina"
cbm_muscle$Tissue <- "Muscle"
cbm_heart$Tissue <- "Heart"
cbm_brain$Tissue <- "Brain"
cbm_kidney$Tissue <- "Kidney"

cbm_tissue <- rbind(cbm_retina[c("Tissue", "Average")],
                    cbm_muscle[c("Tissue", "Average")],
                    cbm_heart[c("Tissue", "Average")],
                    cbm_brain[c("Tissue", "Average")],
                    cbm_kidney[c("Tissue", "Average")])

# Add Tissue column to `cbm_lean_tis`
pattern = c("retina|muscle|heart")
cbm_lean_tis$Status <- "Lean murine"
cbm_obese$Status <- "Obese murine"
df_cbm = rbind(cbm_lean_tis, cbm_obese)
df_cbm$Tissue <- str_to_title(str_extract(df_cbm$Reference, pattern))

# Generate a new dataframe for errorbar (lean vs. obese)
bar_data = data.frame(
  Status = c("Lean murine", "Obese murine"),
  Average = c(rm_cbm_lean_tis$b, rm_cbm_obese$b),
  lb = c(rm_cbm_lean_tis$b - rm_cbm_lean_tis$se,
         rm_cbm_obese$b - rm_cbm_obese$se),
  ub = c(rm_cbm_lean_tis$b + rm_cbm_lean_tis$se,
         rm_cbm_obese$b + rm_cbm_obese$se)
)

# Generate a new dataframe for errorbar (tissue-specific)
bar_data_tis = data.frame(
  Tissue = c("Retina", "Muscle", "Heart", "Brain", "Kidney"),
  Average = c(rm_cbm_retina$b, rm_cbm_muscle$b, rm_cbm_heart$b, rm_cbm_brain$b, rm_cbm_kidney$b),
  lb = c(rm_cbm_retina$b - rm_cbm_retina$se,
         rm_cbm_muscle$b - rm_cbm_muscle$se,
         rm_cbm_heart$b - rm_cbm_heart$se,
         rm_cbm_brain$b - rm_cbm_brain$se,
         rm_cbm_kidney$b - rm_cbm_kidney$se),
  ub = c(rm_cbm_retina$b + rm_cbm_retina$se,
         rm_cbm_muscle$b + rm_cbm_muscle$se,
         rm_cbm_heart$b + rm_cbm_heart$se,
         rm_cbm_brain$b + rm_cbm_brain$se,
         rm_cbm_kidney$b + rm_cbm_kidney$se)
)

# Reorder dataframes
df_cbm$Tissue = factor(df_cbm$Tissue, levels=c("Retina", "Muscle", "Heart", "Brain", "Kidney"))
df_cbm = df_cbm[order(df_cbm$Tissue), ]
cbm_tissue$Tissue = factor(cbm_tissue$Tissue, levels=c("Retina", "Muscle", "Heart", "Brain", "Kidney"))
cbm_tissue = cbm_tissue[order(cbm_tissue$Tissue), ]

# Check the number of rows in each tissue
ncol_retina = nrow(cbm_retina)
ncol_muscle = nrow(cbm_muscle)
ncol_heart = nrow(cbm_heart)
ncol_brain = nrow(cbm_brain)
ncol_kidney = nrow(cbm_kidney)

# Generate colors
colors_status = lightness(c(tail(brewer.blues(3), 1), tail(brewer.greens(3), 1), tail(brewer.oranges(3), 1)), scalefac(0.8))
colors_tis = lightness(c(brewer.blues(ncol_retina), brewer.greens(ncol_muscle), brewer.oranges(ncol_heart),
                         brewer.purd(ncol_brain), brewer.purples(ncol_kidney)), scalefac(0.8))

# Add Color columns to dataframes
cbm_tissue$Colors = colors_tis

# Scatter plot ------------------------------------------------------------
# Compare lean vs. obese (without kidney)
set.seed(1991)
p = ggplot(data = df_cbm, aes(x = Status, y = Average, color = Tissue)) +
  scale_color_manual(breaks=c("Retina", "Muscle", "Heart"), values=colors_status) +
  geom_point(size = 7, position = position_jitter(width=0.1, height=0)) +
  geom_errorbar(data = bar_data, aes(ymin = lb, ymax = ub, y = Average), colour = "black", width=0.2, linewidth=1) +
  geom_point(data = bar_data, aes(x = Status, y = Average), shape = 95, size = 15, colour = "black") +
  xlab("") + ylab(TeX("Capillary basement membrane thickness (nm)")) +
  theme(text = element_text(size = 20)) + ylim(c(0, 350))

show(p)
ggsave(sprintf("%s/cbm_lean_vs_obese.png", results_path), width=2500, height=2500, units="px")
dev.off()
save_as_pdf(sprintf("%s/cbm_lean_vs_obese", results_path), width=6)

# Compare by tissue (without obese group)
p = ggplot(data = cbm_tissue, aes(x = Tissue, y = Average, color = Colors)) +
  scale_colour_identity() +
  geom_point(size = 7, position = position_jitter(width=0.1, height=0)) +
  geom_errorbar(data = bar_data_tis, aes(ymin = lb, ymax = ub, y = Average), colour = "black", width=0.2, linewidth=1) +
  geom_point(data = bar_data_tis, aes(x = Tissue, y = Average), shape = 95, size = 15, colour = "black") +
  scale_x_discrete(limits = c("Retina", "Muscle", "Heart", "Brain", "Kidney")) +
  xlab("") + ylab(TeX("Capillary basement membrane thickness (nm)")) +
  geom_bracket(color = "black", xmin = "Retina", xmax = "Kidney",
               y.position = 560, tip.length = c(0.4, 0.1), label.size = 7, 
               label = generate_plabel(result[[2]]$p.value[4,1])) +
  geom_bracket(color = "black", xmin = "Muscle", xmax = "Kidney",
               y.position = 520, tip.length = c(0.3, 0.1), label.size = 7, 
               label = generate_plabel(result[[2]]$p.value[4,2])) +
  geom_bracket(color = "black", xmin = "Heart", xmax = "Kidney",
               y.position = 470, tip.length = c(0.2, 0.1), label.size = 7, 
               label = generate_plabel(result[[2]]$p.value[4,3])) +
  geom_bracket(color = "black", xmin = "Brain", xmax = "Kidney",
               y.position = 420, tip.length = c(0.1, 0.1), label.size = 7, 
               label = generate_plabel(result[[2]]$p.value[4,4])) +
  geom_bracket(color = "black", xmin = "Retina", xmax = "Heart",
               y.position = 300, tip.length = c(0.1, 0.3), label.size = 7,
               label = generate_plabel(result[[2]]$p.value[2,1])) +
  geom_bracket(color = "black", xmin = "Retina", xmax = "Brain",
               y.position = 360, tip.length = c(0.1, 0.5), label.size = 7,
               label = generate_plabel(result[[2]]$p.value[3,1])) +
  theme(text = element_text(size = 20), legend.position='none') + ylim(c(0, 600))

show(p)
ggsave(sprintf("%s/cbm.png", results_path), width=3500, height=2500, units="px")
dev.off()
save_as_pdf(sprintf("%s/cbm", results_path), width=6)

