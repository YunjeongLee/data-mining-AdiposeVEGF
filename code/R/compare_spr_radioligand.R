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
pkg_list = c("ggplot2", "metafor", "readxl", "weights", "latex2exp", "ggpubr", "RColorBrewer",
             "shades", "ggnewscale", "scales", "ggsignif", "colormap", "dplyr", "stringr", "magick")
instant_pkgs(pkg_list)

# Load data ---------------------------------------------------------------
filename = '../../data/binding_affinity.xlsx'
# VEGF:VEGFR1
vegfr1 <- as.data.frame(read_excel(filename, sheet = "VEGFA165_VEGFR1"))
# VEGF:VEGFR2
vegfr2 <- as.data.frame(read_excel(filename, sheet = "VEGFA165_VEGFR2"))
# VEGF:NRP1
nrp1 <- as.data.frame(read_excel(filename, sheet = "VEGFA165_NRP1"))

# Split dataframes into SPR and radioligand dataframes --------------------
# VEGF:VEGFR1
vegfr1_spr <- vegfr1[vegfr1["Method"] == "SPR", c("Reference", "Kd average", "Kd SE")]
vegfr1_radio <- vegfr1[vegfr1["Method"] == "Radioligand", c("Reference", "Kd average", "Kd SE")]

# VEGF:VEGFR2
vegfr2_spr <- vegfr2[vegfr2["Method...2"] == "SPR", c("Reference...1", "Kd average...3", "Kd SE...4")]
vegfr2_radio <- vegfr2[vegfr2["Method...2"] == "Radioligand", c("Reference...1", "Kd average...3", "Kd SE...4")]

# VEGF:NRP1
nrp1_spr <- nrp1[nrp1["Method"] == "SPR", c("Reference", "Kd average", "Kd SE")]
nrp1_radio <- nrp1[nrp1["Method"] == "Radioligand", c("Reference", "Kd average", "Kd SE")]

# Change column names -----------------------------------------------------
colnames(vegfr1_spr) <- c("Reference", "Average", "SE")
colnames(vegfr1_radio) <- c("Reference", "Average", "SE")
colnames(vegfr2_spr) <- c("Reference", "Average", "SE")
colnames(vegfr2_radio) <- c("Reference", "Average", "SE")
colnames(nrp1_spr) <- c("Reference", "Average", "SE")
colnames(nrp1_radio) <- c("Reference", "Average", "SE")

# Drop empty rows ---------------------------------------------------------
vegfr1_spr <- vegfr1_spr[!is.na(vegfr1_spr["Reference"]), ]
vegfr1_radio <- vegfr1_radio[!is.na(vegfr1_radio["Reference"]), ]
vegfr2_spr <- vegfr2_spr[!is.na(vegfr2_spr["Reference"]), ]
vegfr2_radio <- vegfr2_radio[!is.na(vegfr2_radio["Reference"]), ]
nrp1_spr <- nrp1_spr[!is.na(nrp1_spr["Reference"]), ]
nrp1_radio <- nrp1_radio[!is.na(nrp1_radio["Reference"]), ]

# Delete `Radioligand` and `SPR` from von Tiedemann -----------------------
vegfr1_spr$Reference <- str_remove(vegfr1_spr$Reference, "\\(SPR\\)")
vegfr1_radio$Reference <- str_remove(vegfr1_radio$Reference, "\\(Radioligand\\)")

# Give assumed SE for radioligand assays ----------------------------------
vegfr1_radio[is.na(vegfr1_radio["SE"]), "SE"] <- vegfr1_radio[is.na(vegfr1_radio["SE"]), "Average"] * 0.1
vegfr2_radio[is.na(vegfr2_radio["SE"]), "SE"] <- vegfr2_radio[is.na(vegfr2_radio["SE"]), "Average"] * 0.1
nrp1_radio[is.na(nrp1_radio["SE"]), "SE"] <- nrp1_radio[is.na(nrp1_radio["SE"]), "Average"] * 0.1

# Change unit of Kd for NRP1 from nM to pM --------------------------------
nrp1_spr[c("Average", "SE")] = nrp1_spr[c("Average", "SE")]*1e3
nrp1_radio[c("Average", "SE")] = nrp1_radio[c("Average", "SE")]*1e3

# Data-analysis -----------------------------------------------------------
# Compute weighted average and SD -----------------------------------------
# VEGF:VEGFR1 (SPR)
rm_vegfr1_spr <- rma(yi = Average, sei = SE, data=vegfr1_spr)
summary(rm_vegfr1_spr)

# VEGF:VEGFR1 (Radioligand)
rm_vegfr1_radio <- rma(yi = Average, sei = SE, data=vegfr1_radio)
summary(rm_vegfr1_radio)

# VEGF:VEGFR2 (SPR)
rm_vegfr2_spr <- rma(yi = Average, sei = SE, data=vegfr2_spr)
summary(rm_vegfr2_spr)

# VEGF:VEGFR2 (Radioligand)
rm_vegfr2_radio <- rma(yi = Average, sei = SE, data=vegfr2_radio)
summary(rm_vegfr2_radio)

# VEGF:NRP1 (SPR)
rm_nrp1_spr <- rma(yi = Average, sei = SE, data=nrp1_spr)
summary(rm_nrp1_spr)

# VEGF:NRP1 (Radioligand)
rm_nrp1_radio <- rma(yi = Average, sei = SE, data=nrp1_radio)
summary(rm_nrp1_radio)

# Forest plot -------------------------------------------------------------
# VEGF:VEGFR1 (SPR)
png(file=sprintf("%s/forest_vegfr1_spr.png", results_path), width=1100, height=450)
forest_ylee(data=vegfr1_spr, rm=rm_vegfr1_spr, slab=vegfr1_spr$Reference,
            unit="pM", title="",
            xlab=TeX("Binding affinity, $K_d$ (pM)"), xlim = c(-20, 35), alim = c(0, 20), cex=2, numDigits=1L)
dev.off()
save_as_pdf(sprintf("%s/forest_vegfr1_spr", results_path), width=4)

# VEGF:VEGFR1 (Radioligand)
png(file=sprintf("%s/forest_vegfr1_radio.png", results_path), width=1300, height=550)
forest_ylee(data=vegfr1_radio, rm_vegfr1_radio, slab=vegfr1_radio$Reference,
            unit="pM", title="",
            xlab=TeX("Binding affinity, $K_d$ (pM)"), xlim = c(-150, 280), alim = c(0, 150), cex=2, numDigits=0L)
dev.off()
save_as_pdf(sprintf("%s/forest_vegfr1_radio", results_path), width=4)

# VEGF:VEGFR2 (SPR)
png(file=sprintf("%s/forest_vegfr2_spr.png", results_path), width=1500, height=700)
forest_ylee(data=vegfr2_spr, rm=rm_vegfr2_spr, slab=vegfr2_spr$Reference,
            unit="pM", title="",
            xlab=TeX("Binding affinity, $K_d$ (pM)"), xlim = c(-1200, 2000), alim = c(0, 1100), cex=2, numDigits=0L)
dev.off()
save_as_pdf(sprintf("%s/forest_vegfr2_spr", results_path), width=4)

# VEGF:VEGFR2 (Radioligand)
png(file=sprintf("%s/forest_vegfr2_radio.png", results_path), width=1300, height=700)
forest_ylee(data=vegfr2_radio, rm=rm_vegfr2_radio, slab=vegfr2_radio$Reference,
            unit="pM", title="",
            xlab=TeX("Binding affinity, $K_d$ (pM))"), xlim = c(-800, 1800), alim = c(0, 1000), cex=2, numDigits=0L)
dev.off()
save_as_pdf(sprintf("%s/forest_vegfr2_radio", results_path), width=4)

# VEGF:NRP1 (SPR)
png(file=sprintf("%s/forest_nrp1_spr.png", results_path), width=1300, height=450)
forest_ylee(data=nrp1_spr, rm=rm_nrp1_spr, slab=nrp1_spr$Reference,
            unit="nM", atransf=function(x)x/1e3, title="",
            xlab=TeX("Binding affinity, $K_d$ (nM)"), xlim = c(-7000, 17000), alim = c(0, 10000), cex=2, numDigits=1L)
dev.off()
save_as_pdf(sprintf("%s/forest_nrp1_spr", results_path), width=4)

# VEGF:NRP1 (Radioligand)
png(file=sprintf("%s/forest_nrp1_radio.png", results_path), width=1300, height=700)
forest_ylee(data=nrp1_radio, rm=rm_nrp1_radio, slab=nrp1_radio$Reference,
            unit="nM", atransf=function(x)x/1e3, title="",
            xlab=TeX("Binding affinity, $K_d$ (nM)"), xlim = c(-5000, 9000), alim = c(0, 5000), cex=2, numDigits=2L)
dev.off()
save_as_pdf(sprintf("%s/forest_nrp1_radio", results_path), width=4)

# Student's t-test --------------------------------------------------------
# VEGF:VEGFR1 (SPR vs radioligand)
vegfr1_ttest = wtd.t.test(x=vegfr1_spr$Average, y=vegfr1_radio$Average,
                          weight=1/(vegfr1_spr$SE^2+rm_vegfr1_spr$tau2), 
                          weighty=1/(vegfr1_radio$SE^2+rm_vegfr1_radio$tau2),
                          alternative="two.tailed", samedata=FALSE)

# VEGFR:VEGFR2 (SPR vs radioligand)
vegfr2_ttest = wtd.t.test(x=vegfr2_spr$Average, y=vegfr2_radio$Average,
                          weight=1/(vegfr2_spr$SE^2+rm_vegfr2_spr$tau2), 
                          weighty=1/(vegfr2_radio$SE^2+rm_vegfr2_radio$tau2),
                          alternative="two.tailed", samedata=FALSE)

# VEGFR:NRP1 (SPR vs radioligand)
nrp1_ttest = wtd.t.test(x=nrp1_spr$Average, y=nrp1_radio$Average,
                        weight=1/(nrp1_spr$SE^2+rm_nrp1_spr$tau2), 
                        weighty=1/(nrp1_radio$SE^2+rm_nrp1_radio$tau2),
                        alternative="two.tailed", samedata=FALSE)

# Merge dataframes for plotting -------------------------------------------
vegfr1_radio$Receptor <- "VEGFR1"
vegfr2_radio$Receptor <- "VEGFR2"
nrp1_radio$Receptor <- "NRP1"
vegfr1_spr$Receptor <- "VEGFR1"
vegfr2_spr$Receptor <- "VEGFR2"
nrp1_spr$Receptor <- "NRP1"

vegfr1_radio$Method <- "Radioligand"
vegfr2_radio$Method <- "Radioligand"
nrp1_radio$Method <- "Radioligand"
vegfr1_spr$Method <- "SPR"
vegfr2_spr$Method <- "SPR"
nrp1_spr$Method <- "SPR"

df = rbind(vegfr1_radio[c("Receptor", "Method", "Average")],
           vegfr2_radio[c("Receptor", "Method", "Average")],
           nrp1_radio[c("Receptor", "Method", "Average")],
           vegfr1_spr[c("Receptor", "Method", "Average")],
           vegfr2_spr[c("Receptor", "Method", "Average")],
           nrp1_spr[c("Receptor", "Method", "Average")])

# Define the number of colors
ncol_radio = nrow(vegfr1_radio)+nrow(vegfr2_radio)+nrow(nrp1_radio)
ncol_spr = nrow(vegfr1_spr)+nrow(vegfr2_spr)+nrow(nrp1_spr)

colors_radio = lightness(colorRampPalette(brewer.pal(9,"Blues"))(ncol_radio), scalefac(0.8))
colors_spr = lightness(colorRampPalette(brewer.pal(9,"Reds"))(ncol_spr), scalefac(0.8))

# Add color column
df$Color <- c(colors_radio, colors_spr)

# Group by Receptor
df$Receptor = factor(df$Receptor, levels=c("VEGFR1", "VEGFR2", "NRP1"))
df = df[order(df$Receptor),]

# Generate dataframe including mean and se for errorbar
bar_data = data.frame(
  Receptor = rep(c("VEGFR1", "VEGFR2", "NRP1"), 2),
  Method = c(rep("Radioligand", 3), rep("SPR", 3)),
  Average = c(rm_vegfr1_radio$b, rm_vegfr2_radio$b, rm_nrp1_radio$b,
              rm_vegfr1_spr$b, rm_vegfr2_spr$b, rm_nrp1_spr$b),
  lb = c(rm_vegfr1_radio$b - rm_vegfr1_radio$se,
         rm_vegfr2_radio$b - rm_vegfr2_radio$se,
         rm_nrp1_radio$b - rm_nrp1_radio$se,
         rm_vegfr1_spr$b - rm_vegfr1_spr$se,
         rm_vegfr2_spr$b - rm_vegfr2_spr$se,
         rm_nrp1_spr$b - rm_nrp1_spr$se),
  ub = c(rm_vegfr1_radio$b + rm_vegfr1_radio$se,
         rm_vegfr2_radio$b + rm_vegfr2_radio$se,
         rm_nrp1_radio$b + rm_nrp1_radio$se,
         rm_vegfr1_spr$b + rm_vegfr1_spr$se,
         rm_vegfr2_spr$b + rm_vegfr2_spr$se,
         rm_nrp1_spr$b + rm_nrp1_spr$se),
  Colors = c(lightness(brewer.pal(3, "Blues"), scalefac(0.5)), 
             lightness(brewer.pal(3, "Reds"), scalefac(0.5)))
)

# Scatter plot ------------------------------------------------------------
# Overall SPR vs. radioligand
set.seed(1991)
p = ggplot(df, aes(Method, Average, color=Color)) +
  scale_colour_identity() +
  geom_jitter(width=0.1, height=0, size = 7) +
  geom_errorbar(data = bar_data, aes(ymin = lb, ymax = ub, y = Average), colour = bar_data$Colors, 
                width=0.2, linewidth=1, ) +
  geom_point(data = bar_data, aes(x = Method, y = Average), shape = 95, size = 15, colour = bar_data$Colors) +
  scale_y_log10() +
  scale_y_continuous(transform= 'log10', breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)), limits = c(1e-1, 1e6),
                     sec.axis = sec_axis(transform=~./1e3, name=TeX("Binding affinity, $K_d$ (nM)"),
                                         breaks=trans_breaks('log10', function(x) 10^x),
                                         labels=trans_format('log10', math_format(10^.x)))) +
  xlab("") + ylab(TeX("Binding affinity, $K_d$ (pM)")) +
  facet_wrap(~factor(Receptor, levels=c("VEGFR1", "VEGFR2", "NRP1")), strip.position = "bottom") +
  geom_bracket(data = subset(df, Receptor == "VEGFR1"), aes(Method, Average, color='black'),
               xmin="Radioligand", xmax="SPR", y.position = 3,
               tip.length = c(0.1, 0.4),
               label.size = 7, label=generate_plabel(vegfr1_ttest$coefficients["p.value"])) +
  geom_bracket(data = subset(df, Receptor == "VEGFR2"), aes(Method, Average, color='black'),
               xmin="Radioligand", xmax="SPR", y.position = 3.5,
               tip.length = c(0.05, 0.1),
               label.size = 7, label=generate_plabel(vegfr2_ttest$coefficients["p.value"])) +
  theme(text = element_text(size = 25))

show(p)
ggsave(sprintf("%s/spr_vs_radioligand.png", results_path), width=4000, height=3000, units="px")
dev.off()
save_as_pdf(sprintf("%s/spr_vs_radioligand", results_path), width=7)
