# Load necessary packages
library(tidyverse)
library(caret)
library(missMDA)
library(factoextra)
library(Factoshiny)
library(corrplot)

#**************#
# Visa Premier #
#**************#

# Load dataset
visa_data <- read.csv("Donnees_Bancaires/VisaPremier.txt", sep="\t", row.names=1)

# Relocate target
visa_data <- visa_data %>% relocate(cartevpr, .after = last_col())

# Features selection
# Delete duplicate columns sexe ==> sexer and cartevp ==> cartevpr
visa_data <- visa_data %>% select(-sexe, -cartevp)

# Drop near zero variance columns
nearZeroVars <- nearZeroVar(visa_data, saveMetrics = TRUE)
visa_data <- visa_data[, -nearZeroVar(visa_data)]


cat("The data set has", nrow(visa_data), 
    "observations with", ncol(visa_data),"variables")

# Deal with NA values
any(is.na(visa_data)) # No NAs

# In this data set, there were many variables with '.' as the value
# It was an indication of a missing value which was replaced with NA
# Check '.' values

apply(visa_data, MARGIN = 2, function(x) sum(x == "."))
cols.with.NA = names(visa_data)[apply(visa_data, MARGIN=2, FUN=function(x){ "." %in% x })]

# Replace . with NAS
visa_data <- replace(visa_data, visa_data == ".", NA)

# Transform agemvt and nbpaiecb to numeric
visa_data$agemvt <- as.numeric(visa_data$agemvt)
visa_data$nbpaiecb <- as.numeric(visa_data$nbpaiecb)

# Apply impucePCA
# to impute numeric vars
index <- sapply(visa_data, is.numeric)
ncomp <- estim_ncpPCA(visa_data[, index])
pca.mda <- imputePCA(visa_data[, index], ncp=ncomp$ncp)
visa_data[, index] = pca.mda$completeObs

# Apply imputeMCA 
# to impute categorical vars
index <- sapply(visa_data, is.character)
temp <- imputeMCA(visa_data[, index], ncp=4)
visa_data[, index] <- temp$completeObs

# Delete outliers 
boxplot(visa_data)$out
max.avtscpte <- max(visa_data$avtscpte)
ind <- which(visa_data$avtscpte == max.avtscpte)
visa_data <- visa_data[-ind, ]

#********************
# Preliminary Study #
#********************

data <- visa_data[, -35]
target <- visa_data[, 35]

quali.names <- c("departem", "ptvente", "sitfamil", "csp", "sexer", "codeqlt")
quali.sup.ind <- c(1:length(names(visa_data)))[names(visa_data) %in% quali.names]

data.cor <- cor(data[,-quali.sup.ind])
corrplot(data.cor, hclust.method = "average", type="upper", diag=F)

diag(data.cor) <- 0
t <- which(abs(data.cor) > 0.80, arr.ind=T)
t <- cbind(row.names(data.cor[t[,1], ]), colnames(data.cor[, t[,2]]), data.cor[t])
t[order(as.numeric(t[,3])), ]

# Realiser une ACP
res.pca <- PCA(data, quali.sup=quali.sup.ind, graph = FALSE)

fviz_pca_ind(res.pca, col.ind = as.factor(target), 
             label = "none", addEllipses = TRUE, 
             xlim=c(-7.5,10), ylim=c(-10, 10))

fviz_pca_var(res.pca, repel = TRUE, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

famd_data <- data
for(qual in quali.names){
  famd_data[qual] <- lapply(famd_data[qual], 
                            FUN=function(x){paste(qual, x, sep=".") })  
}

res.FAMD <- FAMD(famd_data)

fviz_famd_var(res.FAMD, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE, labelsize = 4)

fviz_famd_var(res.FAMD, "quali.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE, labelsize = 5)

fviz_famd_ind(res.FAMD, label = "none", col.ind = as.factor(target), 
              addEllipses = T) + xlim(-10, 15) + ylim(-10, 10)

write.csv(file = "VisaPremierAfterPrep.csv", visa_data, fileEncoding = "UTF-8")


