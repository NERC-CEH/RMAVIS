library(vegan)

nvc_pquad_dca <- readRDS(file = "./data/bundled_data/nvc_pquad_dca.rds")

par(mfrow = c(1,1))

grDevices::png(file = "./www/nationalReference.png", width = 1600, height = 1000, bg = "transparent")

plot(nvc_pquad_dca, axes = F, ylab = "", xlab = "", xaxt = "n", yaxt = "n")

grDevices::dev.off()
