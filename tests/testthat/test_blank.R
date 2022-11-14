
setwd("./../../extdata")
blankXlsx="int_blank.xlsx"
nameXlsx="int_qcc.xlsx"

removeBlank(nameXlsx=nameXlsx, blankXlsx=blankXlsx,timeNoise=3,nameWb="correctedWithBlanks.xlsx")
