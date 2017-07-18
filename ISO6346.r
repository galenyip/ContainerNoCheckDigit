validCheckDigit <- function(cntr_no) {
    encodeList <- unlist(strsplit('0123456789A?BCDEFGHIJK?LMNOPQRSTU?VWXYZ', split = ''))
    cntr_no_split = as.list(unlist(strsplit(toupper(cntr_no), split = '')))
    
    # Details on algorithm can be found here https://en.wikipedia.org/wiki/ISO_6346#Check_Digit
    
    # Step 1
    char2num <- sapply(cntr_no_split, function(x, encodeList) {which(x == encodeList)-1}, encodeList = encodeList)
    # Step 2
    sum_encode <- sum(char2num[1:10] * 2^c(0:9))
    # Step 3
    sum_encode_check_digit <- (sum_encode %% 11) %% 10
    
    return (sum_encode_check_digit == char2num[11])
}

calculateCheckDigit <- function(cntr_no) {
    encodeList <- unlist(strsplit('0123456789A?BCDEFGHIJK?LMNOPQRSTU?VWXYZ', split = ''))
    cntr_no_split = as.list(unlist(strsplit(toupper(cntr_no), split = '')))
    
    # Details on algorithm can be found here https://en.wikipedia.org/wiki/ISO_6346#Check_Digit
    
    # Step 1
    char2num <- sapply(cntr_no_split, function(x, encodeList) {which(x == encodeList)-1}, encodeList = encodeList)
    # Step 2
    sum_encode <- sum(char2num[1:10] * 2^c(0:9))
    # Step 3
    sum_encode_check_digit <- (sum_encode %% 11) %% 10
    
    return (sum_encode_check_digit)
}

# Test
cntr_no <- 'CSQU3054383'
result <- validCheckDigit(cntr_no)

cntr_no_2 <- 'OOLU4681105'
result_2 <- calculateCheckDigit(cntr_no_2)


