###Part 1: Core Functions and Fundamentals#######

#In this module, We'll be getting oriented to RStudio and reviewing some of the fundamentals of coding in R. Some of this will be review, though I will be showing you how to apply these skills to working with data, extracting values of interest from results, and beyond.

# 1. RStudio Tips and Basics -----------------------------------


#My recommended RStudio settings:
#Tools-> Global options -> Code -> soft wrap
#Tools-> Global options -> Appearance -> Whatever option looks pretty!
#Tools-> Global options -> Pane Layout -> Whatever feels right!

#First, the hastag is a way of making comments in R. Just like Twitter. #yolo
#This means that R will not run the line of code if it has a hashtag next to it.

This will run, and R will show an error.

#This will not run, and R will not show an error.

# A shortkey for commenting is Highlighting something, then pressing ctrl + shift + c

Highlight
  This
    and
      press
        command + shift + c.

#I will be using comments throughout to explain my code.

#Other Shortkeys:

        #command + Enter --> run line of code
        #command + shift + Enter --> run every line of code ("source")
        #command + shift + r --> Insert section
        #more: https://support.rstudio.com/hc/en-us/articles/200711853-Keyboard-Shortcuts

#New gamechanger: "Show in new window" button!
        
#REALLY helpful starter resources#

        #Cheat sheets: https://www.rstudio.com/resources/cheatsheets/
        
#Here is what you will learn about in this tutorial:
?c()
?rbind()
?cbind()
?rep()
?length()
?example()
?names()
?function(){}
?round()
?mean()
?sd()

#These functions (and many others) will lay the foundation of your understanding, and I will be using all of these for my tutorials.
        
#2. Basic Objects: Vectors, Matrices, and Data Frames ------------------------------------------------------

#Extremerly good resource: http://www.statmethods.net/input/datatypes.html

###Vectors###
        
#When you define something in R, it's called an "object."
#Here is a really basic example:

obj<-"A"
obj

#The "<-" symbol means "assign to". When we run the second line, it just outputs what "obj" is.

#Every object has elements, or parts of it that you can "call" or reference.
#Note that it reads [1] "A". Technically, our object has one element and the first (and only) element is "A".
#Let's add an element by using the c() "combine" function:

obj<-c("A","B")
obj

#This is creating a "vector" of elements, which is a list of values along a single dimension. In particular, this is a 'character' vector because it's elements are all characters (A,B).

#PRO TIP: Want to know more about a function? just put a "?" before an empty function and run it to get more detail. E.g.:

?c()

#Now, we can call different elements within our object. In general, square brackets indicate that we are looking at an element within an object.

#What do you think this code will output?

obj[1]

#that is, we're looking at element 1 of the vector object "obj".

#and this?

obj[2]

#Let's spice things up. Let's say I want to make a table of information, so basically rows AND columns.
#If it's easier, just think of a table as vectors smooshed together. This same logic applies to datasets!!
#Here are two objects:

obj1<-c("A","B","C")
obj2<-c(1,2,3)

#note that obj1 and obj2 are of different classes and types. this can be checked with class() and typeof(), respectively.
class(obj1)
class(obj2)

typeof(obj1)
typeof(obj2)

#So, obj1 is a 'character' vector of type 'character', and ojb2 is a "numeric" vector of type "double".
#Frankly, it helps to know vector classes, but not vector types (e.g., knowing what "double" means does not matter practically).

#It's possible to change the class of an object or series of object. For example you could write:

as.character(obj2)

#to turn the objects into characters, or

as.numeric(obj2)

#to turn it into numbers

as.function(obj2)

#Differentially will turn the numbers into different categories or factors.

#I can combine these objects as rows using the rbind() function:

#r in rbind is for rows
obj.table<-rbind(obj1,obj2)
obj.table

#Now, I have two dimensions in my object: rows and columns.
#In effect, I have created a table here. More accurately, though, I have combined vectors into a "matrix". We can confirm with the class() command:

class(obj.table)

#A matrix is an N x N object, with the SAME object type for each element and the same lengths of the columns. Because we have both numbers and characters, R has defaulted all elements to be of type "character". we can check this with the typeof() command.

typeof(obj.table) #check matrix for element types
typeof(obj.table[1]) #check just one element of matrix

#We'll discuss matrices and data frames as we move along.

#If I wanted to call a specific cell within my table, I would use the convention [Row,Column] instead. (Think RC Cola)

#What value would this output?

obj.table[2,2]

#This is row 2, column 2 in my table.
#There is another way of getting this value too. in my "obj.table" object, this value is also the 4th element:
#up down up down
obj.table[4]

#Let's play around to understand the convention R is using for this:
obj.table[1] #A
obj.table[2] #1
obj.table[3] #B
obj.table[4] #2
obj.table[5] #C
obj.table[6] #3

#Now, what if I wanted the whole row? I would just leave the column part blank and keep the comma:

obj.table[2,]

#Conversely, if I just wanted the whole column:

obj.table[,2]

#We can also omit specific rows/columns using the minus symbol. This is a little tricky to wrap your head around sometimes, so let's practice.
#(NOTE: I seldom use this in practice, but this is helpful information to have in your back pocket.)

obj.table[,-2] #Omits the second column, returns the others
obj.table[-1,] #Omits the first row, returns the other
obj.table[-1,-2] #Omits the first row AND second column, returns the rest

a<-c(3,5,7,2,5)
b<-c(5,12,3,8,8)

#We can also remove multiple rows/comlums using our c() command:

p<-obj.table[,c(-3,-2)] #Removes the third AND second columns, returns a vector.

q<-obj.table[,c(3,2)]

#I can combine my vectors differently using the cbind() function too. For kicks, here is what that would produce:

cbind(obj1,obj2)

#my vectors are just put into columns instead of rows. run obj.table to compare and contrast.

#Note that I did NOT assign the above to anything (remember, with "<-"), so R is just running the code and is not remembering it.

#Everything we've defined so far is in Rstudio's "Global Environment" (to your right by default). This is handy for keeping organized.

################################
#Example: making a usable table#
################################

#We can use this logic to help us create a table.
#First, let's just make a placehodler vector.
#I'm going to use the rep() fucntion to do this, filling a vector with NA's:

a<-rep(NA,5)
a

#This is saying "make a vector of NA's repeated 5 times." I'm calling it "a", but it can literally be anything.
anything<-rep(NA,5) #get it?
anything

#Note that RStudio recognizes NA as the symbol for "not applicable", which is why it turned a different color.

#Now, how do I make a 5 x 5 table? I just bind the vector "a" as a row, 5 times.

table1<-cbind(a,a,a,a,a)
table1

#From here, I can assign row and column names.
#I use the rownames() and colnames() functions for this:
#will get error if row names doesn't match number of rows
rownames(table1) <- c("R1","R2","R3","R4","R5")
colnames(table1) <- c("C1","C2","C3","C4","C5")
table1

#Now that I have an empty table, I just tell R what I want the value of each cell to be.

table1[1,1]<-1
table1[2,1]<-2

#etc.

table1

#You may not always use this in your analyses, but the logic will translate for what you need later. That is, you will need to know how to refer to elements within objects, combine them, manipulate them, etc.

#P.S., the above is the hard way of doing things for illustrative purposes. here is the easy way using the matrix() command:
testmatrix<-matrix(NA,nrow=5, ncol=5)
testmatrix

#3. Lists (AKA, your very best friend) -------------------------------------------------------------------

#Lists are a special class of vectors that are EXTREMELY helpful in understanding data frames and analytic results. A list is just an ordered collection of objects that allow you to flexibly combine a variety of different objects into one place.

#This is the most important thing to know about for doing analyses.

#Let's define a list using the vector() function (remember, a list is just a special vector).
list1<-vector("list",5)
list1

names(list1)<-c("fruits","vegetables","numbers","obj.table")

#To view names, simply use the names() function on a list:

names(list1)#Quiz: why is there an NA at the end of my list object?

list1[["fruits"]]<-c("apples","oranges")
list1[["vegetables"]]<-c("broccoli","carrots","tomatoes?")
list1[["numbers"]]<-seq(1:100)
list1[["obj.table"]]<-obj.table

#There is an even easier way of doing this, using the list convention of adding a dollar sign ($) to the end of your list to call an object or to define something new.

#Call an object:
list1$fruits

#defining something new:
list1$somethingnew<-"test"

#PRO TIP: lists can be an "empty" vector at the start, too. This is a very flexible way of buliding a list if used in tandem with the $ convention.
prolist<-vector("list")
prolist$fruits<-c("apples","oranges")
prolist$vegetables<-c("broccoli","carrots","tomatoes?")
#etc.

#Note that objects within a list do not need to be of the same type. They can be as simple as a number, or as extensive as an entire dataset.
#Here is one way that I could check all the classes of list1 objects at the same time using sapply().

sapply(list1,class)


# The Punchline: Understanding Model Results ------------------------------

#So how does all this help in analysis?
#when you run a model, your results generated are stored as objects, Just like the above.

#Wing it with GRE example from here.

#lm.beta is a command that gives you unstandardized coefficients from your linear model.