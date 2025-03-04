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
subfolders = c("etc", "visualize")
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
             "shades", "ggnewscale", "scales", "ggsignif", "colormap", "magick")
instant_pkgs(pkg_list)

# Load data ---------------------------------------------------------------
filename = '../../data/adipose_tissue_parameters.xlsx'

adipocyte <- as.data.frame(read_excel(filename, sheet = "Adipocyte diameter"))

# Split adipocyte size into two dataframes --------------
# Split adipocyte size dataframe into lean and obese adipose tissue
adipocyte_lean = adipocyte[!is.na(adipocyte["Lean SE...3"]), c("Reference", "Lean average...2", "Lean SE...3")]
adipocyte_obese = adipocyte[!is.na(adipocyte["Obese SE...5"]), c("Reference", "Obese average...4", "Obese SE...5")]

# Change column names -----------------------------------------------------
colnames(adipocyte_lean) <- c("Reference", "Average", "SE")
colnames(adipocyte_obese) <- c("Reference", "Average", "SE")

# Data-analysis -----------------------------------------------------------
# Compute weighted average and SD -----------------------------------------
# Adipocyte diameter of lean mice
rm_adipocyte_lean <- rma(yi = Average, sei = SE, data=adipocyte_lean)
summary(rm_adipocyte_lean)

# Adipocyte diameter of obese mice
rm_adipocyte_obese <- rma(yi = Average, sei = SE, data=adipocyte_obese)
summary(rm_adipocyte_obese)

# Forest plot -------------------------------------------------------------
# Adipocyte diameter of lean mice
png(file=sprintf("%s/forest_adipocyte_diameter_lean.png", results_path), width=1300, height=700)
forest_ylee(data=adipocyte_lean, rm=rm_adipocyte_lean, slab=adipocyte_lean$Reference,
            unit="µm",
            xlab=TeX("Adipocyte diameter (\\mu{m})"), xlim = c(-40, 130), alim = c(10, 80), cex=2, numDigits=0L)
dev.off()
save_as_pdf(sprintf("%s/forest_adipocyte_diameter_lean", results_path), width=4)

# Adipocyte diameter of obese mice
png(file=sprintf("%s/forest_adipocyte_diameter_obese.png", results_path), width=1300, height=900)
forest_ylee(data=adipocyte_obese, rm=rm_adipocyte_obese, slab=adipocyte_obese$Reference,
            unit="µm",
            xlab=TeX("Adipocyte diameter (\\mu{m})"), xlim = c(-50, 200), alim = c(20, 120), cex=2, numDigits=0L)
dev.off()
save_as_pdf(sprintf("%s/forest_adipocyte_diameter_obese", results_path), width=4)

# Student's t-test --------------------------------------------------------
adipocyte_lean_vs_obese = wtd.t.test(x=adipocyte_lean$Average, y=adipocyte_obese$Average,
                                     weight=1/(adipocyte_lean$SE^2+rm_adipocyte_lean$tau2), 
                                     weighty=1/(adipocyte_obese$SE^2+rm_adipocyte_obese$tau2),
                                     alternative="less", samedata=FALSE)

# Merge dataframes for plotting -------------------------------------------
adipocyte_lean$Source <- "Lean mice"
adipocyte_obese$Source <- "Mice with obesity"

adipocyte = rbind(adipocyte_lean[c("Source", "Average")],
                  adipocyte_obese[c("Source", "Average")])

# Error bar data
bar_data = data.frame(
  Source = c("Lean mice", "Mice with obesity"),
  Average = c(rm_adipocyte_lean$b, rm_adipocyte_obese$b),
  lb = c(rm_adipocyte_lean$b - rm_adipocyte_lean$se,
         rm_adipocyte_obese$b - rm_adipocyte_obese$se),
  ub = c(rm_adipocyte_lean$b + rm_adipocyte_lean$se,
         rm_adipocyte_obese$b + rm_adipocyte_obese$se),
  color = c("darkblue", "darkred")
)

# Scatter plot ------------------------------------------------------------
set.seed(1991)
p = ggplot() +
  geom_point(data = adipocyte_lean, aes(x = "Lean mice", y = Average, colour = Reference), size = 7, position = position_jitter(width=0.1, height=0)) +
  geom_errorbar(data = bar_data, aes(x = Source, ymin = lb, ymax = ub), colour = bar_data$color, width = 0.15, linewidth=1) +
  geom_point(data = adipocyte_lean, aes(x = "Lean mice", y = rm_adipocyte_lean$b), shape = 95, size = 20, colour = "darkblue") +
  labs(color="Lean mice") +
  lightness(scale_color_brewer(palette="Blues"), scalefac(0.8)) +
  guides(color = guide_legend(order=1)) +
  new_scale_color() + 
  geom_point(data = adipocyte_obese, aes(x = "Obese mice", y = Average, colour = Reference), size = 7, position = position_jitter(width=0.1, height=0)) +
  geom_point(data = adipocyte_obese, aes(x = "Mice with obesity", y = rm_adipocyte_obese$b), shape = 95, size = 20, colour = "darkred") +
  lightness(scale_color_colormap('Mice with obesity', discrete = T,colormap = "freesurface-red", reverse = T), scalefac(0.8)) + 
  xlab("") + ylab(TeX("Adipocyte diameter (\\mu{m})")) +
  geom_bracket(data = adipocyte, aes(x = Source, y = Average), xmin = "Lean mice", xmax = "Mice with obesity",
               y.position = 110, tip.length = c(0.5, 0.1), label.size = 7, 
               label = generate_plabel(adipocyte_lean_vs_obese$coefficients["p.value"])) +
  theme(text = element_text(size = 20)) + ylim(c(0, 150))

show(p)
ggsave(sprintf("%s/adipocyte_diameter_lean_vs_obese.png", results_path), width=3500, height=2500, units="px")
dev.off()
save_as_pdf(sprintf("%s/adipocyte_diameter_lean_vs_obese", results_path), width=6)
