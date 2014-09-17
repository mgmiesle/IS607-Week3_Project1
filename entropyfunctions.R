# Matthew Miesle
# CUNY SPS - IS 607
# Week 3 Project
# Due: 2014-09-16

entropy <- function(vectd)
{
    fvectd <- as.factor(vectd)
    levs <- levels(fvectd)
    numlevels <- length(levs)
    numitems <- length(fvectd)
    numeachitem <- c()
    for (iter in 1:numlevels)
    {
        numeachitem[iter] <- length(fvectd[fvectd == levs[iter]])
    }
# is there a better/vectorized way to assign numeachitem?
    p = numeachitem/numitems
    Esub <- ifelse(p == 0, 0, - p * log2(p))
    (E <- sum(Esub))
}

infogain <- function(igvectd, igattriba)
{
    Esystem <- entropy(igvectd)
    fvecta <- as.factor(igattriba)
    levs <- levels(fvecta)
    numlevels <- length(levs)
    vectdlist <- list(c())
    for (iter in 1:numlevels)
    {
        (vectdlist[[iter]] <- igvectd[igattriba == levs[iter]])
    }
# is there a better/vectorized way to assign vectdlist[[]]?
    Esub <- sapply(vectdlist, entropy)
    sumEsubs <- sum((sapply(vectdlist, length) / length(fvecta)) * Esub)
    (igain <- Esystem - sumEsubs)
}

decide <- function(daa, colnum)
{
    gains <- c()
    for (iter in 1:(ncol(daa)))
    {
        ifelse(iter == colnum, gains[iter] <- 0, gains[iter] <- infogain(daa[, colnum], daa[, iter]))
    }
# is there a better/vectorized way to assign gains?
    names(gains) <- names(dataset)
    (list(max = which(gains == max(gains)), gains = gains[gains != 0]))
}

filepath <- "C:/Users/MattM/Downloads/entropy-test-file.csv"
dataset <- read.table(file = filepath, header = TRUE, sep = ",")

entropy(dataset$answer)
infogain(dataset$answer, dataset$attr1)
infogain(dataset$answer, dataset$attr2)
infogain(dataset$answer, dataset$attr3)
decide(dataset, 4)

# Expected output:
# 0.9832692
# 2.411565e-05
# 0.2599038
# 0.002432707
# $max
# [1] 2
# $gains
# attr1 attr2 attr3
# 2.411565e-05 2.599038e-01 2.432707e-03