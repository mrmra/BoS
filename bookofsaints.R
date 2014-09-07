saints <- data.frame('NAME'=0,'DOBNEAR'=0,'DECEASEDNEAR'=0,'OCCUPATION'=0,'DESCRIPTION'=0)
saints <- saints[-1,]
dataLines <- readLines("oxfordmarkoneline.txt", n=-1, encoding="ANSI")
dataList <- strsplit(dataLines, "|", fixed=TRUE)
dataVector <- unlist(dataList)
rm(dataList)
rm(dataLines)
for(i in 1:length(dataVector)){
    ## read in data
    tempValue=dataVector[i]
    
    ## pull names into column
    tempValue2 <- unlist(strsplit(tempValue, ',', fixed=TRUE))
    m <- gregexpr("[A-Z ]*(?=\\()", tempValue2[1], perl=TRUE)
    tempName <- regmatches(tempValue2[1], m)
    print(tempName)
    if(nchar(tempName) > 0){
        saints[i,'NAME'] <- tempName
    }
    else saints[i,'NAME'] <- NA

    ## pull occupation into column
    m <- gregexpr("\\),[ A-z]*(?=\\.)", tempValue, perl=TRUE)
    tempName <- unlist(strsplit(as.character(regmatches(tempValue, m)), ',', fixed=TRUE))
    if(nchar(tempName[2]) > 0){
        saints[i,'OCCUPATION'] <- tempName[2]
    }
    else saints[i,'OCCUPATION'] <- NA

    ## pull description into column
    tempValue2 <- unlist(strsplit(tempValue, '.', fixed=TRUE))
    n <- length(tempValue2)-1
    tempValue3 <- paste0(tempValue2[2:n], collapse=".")
    if(nchar(tempValue3) > 0){
        saints[i,'DESCRIPTION'] <- tempValue3
    }
    else saints[i,'DESCRIPTION'] <- NA
    
    ## pull dates to columns. Get between (), unlist, take the first, split
    ## by '-'. 'If' statements to figure out birth/death/NA.
    m <- gregexpr("(?<=\\()(.*?)\\)", tempValue, perl=TRUE)
    tempValue2 <- unlist(regmatches(tempValue,m))
    n=nchar(tempValue2[1])-1
    tempValue2 <- substr(tempValue2[1],1,n)
    tempValue2 <- unlist(strsplit(tempValue2, '-', fixed=TRUE))
    
    if(nchar(tempValue2[1]) > 0 && is.na(tempValue2[2])){
        saints[i,'DECEASEDNEAR'] <- tempValue2[1]
        saints[i,'DOBNEAR'] <- NA
    }
    if(nchar(tempValue2[1]) > 0 && !is.na(tempValue2[2])){
        saints[i,'DECEASEDNEAR'] <- tempValue2[2]
        saints[i,'DOBNEAR'] <- tempValue2[1]
    }
    if (tempValue2[1] == '?') {
        saints[i,'DECEASEDNEAR'] <- NA
        saints[i,'DOBNEAR'] <- NA
    }
    
}
rm(tempValue)
rm(tempValue2)
rm(tempName)
rm(tempValue3)
rm(n)
write.csv(saints,"saints.csv")