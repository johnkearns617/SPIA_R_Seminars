# 1_The_Basics.R
# John Kearns
# Goal: Show the basics of R code, including computations
# Date Created: 2023-10-06
# Last Updated: 2023-10-06

# File structure
# Here is where you would put some information on what folders you may be accessing in this do-file

# Load Packages
# This would be where you would bring in ALL packages that you would use in this do-file
library(tidyverse)

# Load first data file

# Code

#### Basic arithmetic ####

# What do these do?
2+2

5*4/3

(2+2)*4^3

8%%7


#### Assigning variables ####
x = 3
x

x = 5
x

y = "Hello"
y

z = (2+2)*4^3
z

w = x/z
w

x+z

y+z
y+y


2:5/3  # what is this doing?

1object = "don't do this"

&object = "or this"

paste = "or this either"


#### Printing and Pasting ####

# print() will spit out the output of any function into the console
print("Hello World")

print("Hello","World")

# paste() should be used for combining strings
paste("Hello World")

paste("Hello","World")

?paste()

# sep inserts spaces in here
paste("Hello","World","This","is","R",sep=" space ")

# collapse will combine multiple strings
paste(c("Nassau","Witherspoon","Alexander"))

paste(c("Nassau","Witherspoon","Alexander"),collapse="; ")

# cat doesn't save any objects
cat1 = cat(y,y)
cat1 


#### vectors ####
# vectors are one-dimensional objects (think list a collection of values, or a column in a data frame)
# c() adds things into a vector

vector1 = c("Nassau","Witherspoon","Alexander")

print(state.abb)

#### slicing ####
# use the [] with a vector or data frame to select different parts

state.abb[13] # returns the 13th state in the list
state.abb[23:47] # 23rd to 47th states


#### append ####
list1 = c(1,2,3,4,5)
list2 = append(list1,c(6,7,8))


#### data frames ####
number_df = data.frame(column_1=c(1,2,3),column_2=c(4,5,6),column_3=c(7,8,9))

number_df = rbind(number_df,c(10,11,12))

colnames(number_df)
colnames(number_df) = paste0("col",c(1:3))
colnames(number_df)[3] = "new"

number_df$column_4 = c("a","b","c") # why doesn't this work?

number_df$column_4 = c("a","b","c","d")

number_df$col1[-1]

number_df$new[2:3]

number_df$new[c(2,4)]

number_df[2:3,] # slices the 2nd and 3rd rows for all columns

number_df[2:3,3] 

number_df[2:3,"new"]

# when slicing a data frame, you need to either:
# (1) specific row and columns selected if using df[#,#]
# (2) specify one column when slicing, like df$column[#]


number_df = cbind(number_df,number_df)


# be careful about overwriting objects


#### Misc. functions ####

seq(from=1,to=100,by=8)
seq(from=1,by=2,length.out=10)

Sys.Date()

# Save what you were working on
