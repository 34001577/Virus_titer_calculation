#We assume our virus titer is 1e8/mL(infectious unit).
#Based upon the dilution factor and cell number(50,000/well) used for transduction.
#We can extrapolate the theoretical MOI for each test well.
#The MOIns,with descend ranking, are 6.667,2.222,0.741,0.247,0.082,0.0273
 
#########THE ONLY THING YOU NEED TO DO IS IMPORT THE DATA########


source("https://raw.githubusercontent.com/34001577/Virus_titer_calculation/main/Titer_calculation_function_source_code.R?token=AV6KZKG47B7ITBYZSLOMLR3BLX63I")

#read your in xlsx file

data <- readxl::read_xlsx("titeration percentage.xlsx")#read your dataset


Titer_calculation_plot(data = data, ncol = 2, nrow = 2)#nrow and ncol defines how you arrange your graphs.



