# Matthew Miesle
# CUNY SPS - IS 607
# Week 3 Project
# Due: 2014-09-16

#Entropy Function
entropy <- function(vectd)
{
#     cast input to type factor
#     ***** if p == 0 then E for that entry should be 0
#     find different levels within the factor
#     need the number of levels
#     each sum will be calculated individually (for loop is as long as number of items)
#     count the number of level appearances within the vector for each level
#     and divide by the total number of items in the vector (length of vector)
#     need to understand how to perform logs
    fvectd <- as.factor(vectd)
    levs <- levels(fvectd)
    numlevels <- length(levs)
    numitems <- length(fvectd)
    numeachitem <- c()
    p <- c()
    Esub <- c()
    for (iter in 1:numlevels)
    {
        numeachitem[iter] <- length(fvectd[fvectd == levs[iter]])
        p[iter] <- numeachitem[iter]/numitems
        ifelse(p[iter] == 0, Esub[iter] <- 0, Esub[iter] <- - (p[iter] * log2(p[iter])))
    }
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#     ***** if p == 0 then E for that entry should be 0
#     this is performed by the ifelse statement above
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#     (E <- - (sum(p * log2(p))))
    (E <- sum(Esub))
    
            
}

infogain <- function(igvectd, igattriba)
{

#     run entropy on entire group and then again for the individual subset
    Esystem <- entropy(igvectd)

    fvecta <- as.factor(igattriba)
    levs <- levels(fvecta)
    numlevels <- length(levs)
#     numitems <- length(fvectd)
#     numeachitem <- c()    

    vectdlist <- list(c())
    Esub <- c()
    sumEsubs <- 0

    for (iter in 1:numlevels)
    {
        (vectdlist[[iter]] <- igvectd[igattriba == levs[iter]])
        (Esub[iter] <- entropy(vectdlist[[iter]]))
        (sumEsubs <- sumEsubs + (length(vectdlist[[iter]]) / length(fvecta)) * Esub[iter])
    }

#     should it be (length(vectdlist[[iter]]) / length(igvectd)) * Esub[iter])  ???

# sumEsubs should be 0.9832451 for dataset$attr1 according to solution
    # create a vector for each level of igvectd that is filled with values of igattriba
    # run the entropy on each of these vectors
    
    
#     Eda <- (%1) * E1 + (%2) * E2 + ...
#     Eattrib <- entropy(igattriba)
#     igain <- Esystem - Eda  
    (igain <- Esystem - sumEsubs)
    
}

decide <- function(daa, colnum)
{
    gains <- c()
    for(iter in 1:(ncol(daa)))
    {
        ifelse(iter == colnum, gains[iter] <- 0, gains[iter] <- infogain(daa[, colnum], daa[, iter]))
#         gains[iter] <- infogain(daa[colnum], daa[iter])
    }
    
#     max(gains)
#   
#     gains
    (list(max = which(gains == max(gains)), gains = gains[gains != 0]))

#     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#     need to get column names from input data frame to be the column names of the gains list
#     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    # loop through to run infogain on each column with respect to the chosen column
    # infogain of all columns as vector <- 
    # maxinfogain <- max(infogain of all columns as vector)
    # which column # is maxinfogain in?
    #     returnlist <- (column # of attribute that maximizes infogain,
    #     vector of info gains for each attribute)

}

#     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#     algorithm that uses some recursion with entropy() inside infogain() ?????
#     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#     algorithm that uses any apply functions to vectorize the process?
#     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# need to import dataset
# C:/Users/MattM/Downloads/entropy-test-file.csv
filepath <- "C:/Users/MattM/Downloads/entropy-test-file.csv"
dataset <- read.table(file = filepath, header = TRUE, sep = ",")

entropy(dataset$answer)
infogain(dataset$answer, dataset$attr1)
infogain(dataset$answer, dataset$attr2)
infogain(dataset$answer, dataset$attr3)
decide(dataset, 4)