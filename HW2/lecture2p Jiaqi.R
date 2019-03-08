# Problem Set 2 Solution Template
# mysoln is a list to store your answers

# the first element of the list is a vector of names for the students in your group
# make sure these match the names shown on the pdf document provided by the MFE office
# using group 1 as an example:
mysoln = list(student = c("Molin Liang", "Meghana Rao", "Chengbo Du", "Shardul Kulkarni"))

# 1

# your intermediary code to get your answers here

#a
100/(1+0.06)^3


#b
6/(1+0.05) + 106/(1+0.055)^2

#c
8/(1+0.05) + 8/(1+0.055)^2 + 8/(1+0.06)^3 + 108/(1+0.063)^4

# save down your final answers for part a, b, and c
a = c(83.96193,0.06) #ytm in decimal form
b = c(100.9502,0.055) #ytm in decimal form
c = c(106.1081,0.062) #ytm in decimal form

# add answers to list for "Q1"
mysoln[["Q1"]] = list(a=a, b=b, c=c)

# 2

# your intermediary code to get your answers here

2 *(102/100.98 - 1)

# save down your final answers
a = c(0.02, 0.024) #in decimal form

# add answers to list for "Q2"
mysoln[["Q2"]] = list(a=a)

# 3
# your intermediary code to get your answers here

# save down your final answers

# Put the answer in your PDF writeup
