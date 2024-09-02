dataFile <- file.path("/Users/KangDong/Desktop/Lab_app/data/Angiogenesis Panel 1-Plate 1.csv")
standardFile <- file.path("/Users/KangDong/Desktop/Lab_app/data/Angiogenesis Panel 1-Plate 1-Standard.csv")
splFile <- file.path("/Users/KangDong/Desktop/Lab_app/data/Sample_Manifest.xlsx")
msd <- preprocessMSD(dataFile, standardFile)

spl <- read_excel(splFile, sheet = "Sample Manifest")
metadata <- spl %>% left_join(msd, by = c("SAMPLE" = "sample"))

plotCV(metadata %>% filter(assay == "PIGF"))

test_data <- metadata %>% filter(assay == "PIGF")
plotQC(test_data)
