###Data Visualization#######


# 1. Setup ----------------------------------------------------------------

#Let's start with our basic data setup. I will actually be using a mostly-clean SPSS file this time since this is likely the form that your data files will usually take.
rm(list = ls())
options(scipen=20)
options(digits=4) #show 4 digits when displaying results (e.g., 0.000 or 00.00)

# source() a function that just grabs a file and reads everything in it. Useful for creating a list of things that just load commands you've made automatically

#Run the following line ONLY if you have not installed the below packages

# install.packages(c("dplyr","ggplot2", "gridExtra"))

gre.gpa<-read.csv("https://raw.githubusercontent.com/connorjmccabe/Regression_523.525_2018/master/GPA_GRE_Q.csv")

#Reading in a file from my local computer:
setwd("~/Dropbox/UW Stats WD")
setwd("~/Desktop/UW/Winter '18/Linear Models & Data Analysis/523/WD") 

#Since I've set my working directory, I no longer need to specify a file location.
#Note that, unless otherwise specified, all files I save to my working directly unless otherwise specified.

gre.gpa<-read.csv("GPA_GRE_Q.csv")

#Please edit your setwd() to your own preferred directory on your own time. For now, let's read in the data file from the GitHub location:

gre.gpa<-read.csv("https://raw.githubusercontent.com/connorjmccabe/Regression_523.525_2018/master/GPA_GRE_Q.csv")

#The package dplyr is a great resource for easily cleaning up data (data manipulation/data fixing). the basic format is that you specify your data file, then type %>% to indicate what manipulations you plan on doing (this is called "piping"). Whenever you see %>% here, just think that it really means "and then..."

require(dplyr)
gre.gpa<- gre.gpa %>% #re-define my data as itself, and then...
       mutate_all(funs(as.numeric), c("College.GPA","GRE.Q"))
  #apply as.numeric to GPA and GRE (similar to sapply (simply apply, as opposed to lapply (apply to a list)), but for data frames)


#Below, 
gre.gpa.scaled<- gre.gpa %>% #define object gre.gpa.scaled, and then...
  transmute_all(funs(scale)) #standardize (get z score) by centering at zero and diving by the standard deviation

# Alternatively you could write the following line of code to do everything at on e
#   gre.gpa.scaled<- gre.gpa %>%
#      mutate_all(funs(as.numeric), c("College.GPA","GRE.Q")) %>%
#      transmute_all(funs(scale))

#Honestly, I'm not too familiar with dplyr, but this could be a great resource. Cheatsheet:

#https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
  

# 2. Basics of ggplot2 ----------------------------------------------------

#Great resource: http://www.r-bloggers.com/r-tutorial-series-graphic-analysis-of-regression-assumptions/

#Full disclosure: I find coding displays in R can be a collosal pain. The results are beautiful and you can defniitely do some high-powered stuff, but there can be (and literally is) a full class on how to display your data. Coding in ggplot2, the most popular data vis package, is like it's own language, and doesn't always translate well from basic R coding conventions. Thus, making graphics can be a pain, but the end result is well worth it in my opinion. I will focus on the basics here, giving you the tools for creating your own awesome graphics moving forward.

#There is a base plotting package in R, but I will be covering it minimally; ggplot2() is the most popular plotting package out there right now, and for good reason. Let's have a look-see.

#First, let's get oriented to how ggplot2 works, without using data just yet.
#ggplot2 uses "layers" to build a plot to one's liking. Think of it like a canvas with layers of paint on top.
#Here is the most basic plot you can make - a blank canvas.
require(ggplot2)

ggplot()

#Note that we have a big grey "canvas" here, because ggplot defaults to a grey background. My first recommendation: use theme_bw() in all your graphics. ggplot2 has multiple pre-packaged themes that you can expolore using, or you can build your own. more on this later.
#The convention for adding layers to your plots is using a "+" sign. So, let's add a theme layer to this.

ggplot() + 
  theme_bw()

#A personal recommendation for efficiency and clarity: add layers using separate lines, like the above. Then, when you're exploring the best-looking options for a graphic, you can just comment different portions out to quickly explore what works best for you. Also, if you have a layer that you will have in every one of your plots (such as theme_bw), keep that as your LAST line without a period. This prevents your plots from throwing errors unecessarily.

#let's add some more layers to this, taking some examples of 

qplot(College.GPA, data = gre.gpa) + theme_bw()  #qplot stands for quick plot, it's useful for getting quick and dirty histograms for you data

ggplot() +
  geom_histogram(data=gre.gpa, aes(x=College.GPA)) + 
  theme_bw()

#This is the functional equivelant to the above qplot code

ggplot() +
  xlim(-3,3) + #set the limits of the x-axis to -3 and 3
  ylim(-3,3) + #set the limits of the y-axis to -3 and 3 as well
  geom_hline(yintercept = 0) + #put a horizontal line at y=0
  geom_vline(xintercept = 0, linetype = "dashed") + #put a vertical dashed line at x=0
  # geom_vline(xintercept = 0, linetype = "solid") +
  theme_bw()

#Curious about different specifications? Google and the help options are your ally. E.g., what options do I have for different linetypes?

?linetype

#This is a decent starting point for building our graphics. you can even wrap these specifications into an object, put it into your functions file list (e.g, Connor_R_functions.r) that you can pull in anytime you want.

#Here is a theme I pulled directly from the internet that (allegedly) gives you APA formatted specifications:

apatheme<-theme_bw()+
  theme(
    panel.grid.major=element_blank(), #No major gridlines
        panel.grid.minor=element_blank(), #No minor gridlines
        panel.border=element_blank(), #No panel border
        axis.line=element_line(), #No axis lines
        legend.title=element_blank() #No legend
        )

#now, I can run this in place of theme_bw:

ggplot() +
  xlim(-3,3) +
  ylim(-3,3) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, linetype = "dotdash") +
  # geom_vline(xintercept = 0, linetype = "solid") +
  apatheme

# Not to your liking? you can simply comment or change elements that aren't to your fancy. E.g., how might I change apatheme to show me panel borders moving forward? (Hint: I can do it with one keystroke)

#Useful for sharing, you can also define a plot (I call it a "baseplot") and add additional elements stepwise from there, e.g.:

baseplot <- ggplot()
baseplot #blank canvas.
baseplot1 <- baseplot + xlim(-3,3) + ylim(-3,3)
baseplot1 #blank canvas with x and y limits.
baseplot2 <- baseplot1 + geom_hline(yintercept = 0)
baseplot2 #blank canvas with x and y limits and a horizonal line at 0 for some reason.
#etc.
baseplot3<-baseplot2 + apatheme
baseplot3
# 3. Using ggplot with data ---------------------------------------------------------

#Let's start with a bivariate scatterplot. First, let's see what happens when we run the below line:

ggplot(data=gre.gpa, aes(x=College.GPA, y=GRE.Q))

#why the above and not the below? It allows you to make changes and is easier to navigate quickly and comment upon

ggplot() +
  geom_point(data=gre.gpa, aes(x=College.GPA, y=GRE.Q)) +
  theme_classic()

#Note that ggplot asks for a few entries: the dataset referenced and the variables used. aes() means "aesthetics". basically, it's what defines the x and y axes.

#Why isn't it showing us anything? Because we need to add layers!

#Since we're making a scatterplot, we need to add a "geom" layer called "geom_point()"
  
ggplot(data=gre.gpa, aes(x=College.GPA, y=GRE.Q)) +
  geom_point() +
  theme_bw()

#Note that this is NOT my linear model, but rather a bivariate scatterplot betweeen GPA and GRE.
#in the ggplot function, we are telling ggplot to look in dataset gre.gpa for "aesthetics" x=GPA and y=GRE. That is, we are building a graphic with ss on the x axis and negcon on the y axis.

#Aesthetics can take at least 5 primary modes. Think of each like a "dimension" of your data that is added to the graphic. The first two dimensions above are nearly always in plots and are the spatial dimensions - x and y. essentially, we are saying in the above "put GPA on the x axis and GRE on the y axis).

#Here are other critical aesthetics:
?color
?fill
?group

#Let's pretend we have gender in our dataset (we'll call this variable "female").
#An aside: this is one way I can create a gender varaible, where 0=male, 1=female, and I want 40% (ish) of my sample to be female.

gre.gpa$female<-rbinom(n=nrow(gre.gpa),size = 1,prob = .4) 

#Does anyone have a guess as to why I called this variable "female" instead of "gender"? Whatever 1 represents you should label it as

gre.gpa$female<-as.factor(gre.gpa$female)
#Any guesses why I re-defined female as a factor here? Factor = categorical in R. An organizing variable

#Color donotes what aesthetic will be described based on differences in color, e.g.:
ggplot(gre.gpa, aes(x=College.GPA, y=GRE.Q, color = female)) +
  geom_point() +
  scale_color_manual(values=c("green", "blue")) + #useful for assigning color
  #geom_point(colour = 'green')  #is an alternative to the above if you need just one color assigned
  theme_bw()

#If you wanted to do it in a more comprehensive way you could define the variables like before
gre.gpa$female<-rbinom(n=nrow(gre.gpa),size = 1,prob = .4) 
#then specify labels for your categories
gre.gpa$female[gre.gpa$female=="0"]<-"male"
gre.gpa$female[gre.gpa$female=="1"]<-"female"
#then rename the variable
gre.gpa$Gender<-gre.gpa$female
gre.gpa$female<-NULL

View(gre.gpa)
#then finally run the plot as before
ggplot(gre.gpa, aes(x=College.GPA, y=GRE.Q, color = Gender)) +
  geom_point() +
  scale_color_manual(values=c("green", "blue")) + #useful for assigning color
  #geom_point(colour = 'green')  #is an alternative to the above if you need just one color assigned
  theme_bw()

#another function that helps assign colors require(RColorBrewer)
#
#Also, googling R color pallette will help you figure out what colors ggplot takes
#Now, we can distinguish points for each femaleder based on color. This is ugly still, but we'll build from here.

#Fill is another color aesthetic, but refers specifically to what the inside of a figure's color will be.
ggplot(gre.gpa, aes(x=College.GPA, y=GRE.Q, shape = female)) +
  geom_point() +
  theme_bw()


#We have yet to specify limits of the x and y axis, so ggplot takes College.GPA at these based on the range of each of these variables.


#When the aesthetics of a layer are blank, it will aCollege.GPAume that the aesthetics match that which is specified in ggplot(). Also note that I left the geom_point function blank without any arguments, so geom_point() gives me the default settings available. This is nice when you want a quick and dirty graphic, though there are manny specifications you can add to this. If I want more information about geom_point options, what can I do?

#Here are some ways I can doll it up a bit more to my liking: 
ggplot(gre.gpa, aes(x=College.GPA, y=GRE.Q)) +
  geom_point(size = .5, color = "blue", shape = 16, alpha = .5) + #size, color, and shape refer to the size, color, and shape of the points (no way!), and alpha refers to how transparent your points are. I use this to help audiences quickly identify where the data are clustered (overlapping points make areas that are darker).
  theme_bw()

#from here, we can pretty it up with the many, many options that ggplot2 gives us to change how it looks. I will not go through all these options, as they are EXTREMELY numerous. Instead, I will provide you with some of my favorite resources for helping me build plots:

#For theme() settings: http://docs.ggplot2.org/0.9.2.1/theme.html
#Diagnostics et al.: http://www.ats.ucla.edu/stat/r/modules/exploring_w_graphics.htm
#geom cheat sheet: http://sites.uci.edu/stat67/files/2014/08/ggplot2-cheat-sheet-two-page.pdf
#Tickmarks and labels: http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels
#EXCELLENT shape guides: 
#http://sape.inf.usi.ch/quick-reference/ggplot2/shape
#http://www.sthda.com/english/wiki/ggplot2-point-shapes
#Potential resource on making gifs (have not used this yet): http://www.math.yorku.ca/SCS/spida/lm/visreg.html

#Here is an example plot I made the hard way, where I manually specified pretty much every aspect I was interested in:

gre.gpa.scatter<-ggplot(gre.gpa, aes(x=College.GPA, y=GRE.Q)) +
  geom_point(shape=16, size = 1) +
  ylim(min(gre.gpa$GRE.Q,na.rm=T) - sd(gre.gpa$GRE.Q,na.rm=T),max(gre.gpa$GRE.Q,na.rm=T) + sd(gre.gpa$GRE.Q,na.rm=T)) + #this gives y axis limits. What's a savvier way of doing this?
#Here, I specify all my nitty-gritty details to pretty it up.
   theme(text=element_text(size=16, color="black"), #Use size 30 black text
        # axis.text.x=element_text(colour="black"), #dont need because of element_text
        # axis.text.y=element_text(colour="black"), #dont need because of element_text
        axis.title.y=element_text(size=20), #change y axis title size to be a bit bigger than everything else
        panel.background = element_rect(fill = 'white'), #make plot background white
        panel.grid.major = element_line(colour="#C0C0C0"), #make major grid panel a specific shade of grey (of of fifty).
         # panel.grid.minor = element_line(colour="#C0C0C0"), #set minor grid panels to that color too
         # lefemaled.background= element_rect(fill="white", colour=NA), #Don't need
         plot.background=element_rect(fill='white')) + #make plot background white
  labs(x = "GPA", y= "GRE \n Quantitiative") #\n tells the code to place the next component on the new line thing on a new line

gre.gpa.scatter

#If I'll be using this template later, I can write myself a function to save my specifications. E.g.:

cmplot<-function(df, xvar, yvar){
  ggplot(data=df, aes(x=xvar,y=yvar)) +
  geom_point(shape=16, size = 1) +
    ylim(min(yvar,na.rm=T) - sd(yvar,na.rm=T),max(yvar,na.rm=T) + sd(yvar,na.rm=T)) + #this gives y axis limits. What's a savvier way of doing this?
    xlim(min(xvar,na.rm=T) - sd(xvar,na.rm=T),max(xvar,na.rm=T) + sd(xvar,na.rm=T)) + #this gives y axis limits. What's a savvier way of doing this?
    #Here, I specify all my nitty-gritty details to pretty it up.
    theme(text=element_text(size=16, color="black"), #Use size 30 black text
          # axis.text.x=element_text(colour="black"), #dont need because of element_text
          # axis.text.y=element_text(colour="black"), #dont need because of element_text
          axis.title.y=element_text(size=20), #change y axis title size to be a bit bigger than everything else
          panel.background = element_rect(fill = 'white'), #make plot background white
          panel.grid.major = element_line(colour="#C0C0C0"), #make major grid panel a specific shade of grey (of of fifty).
          # panel.grid.minor = element_line(colour="#C0C0C0"), #set minor grid panels to that color too
          # lefemaled.background= element_rect(fill="white", colour=NA), #Don't need
          plot.background=element_rect(fill='white')) + #make plot background white
    labs(x = "GPA", y= "GRE \n Quantitiative")
}

  plot.unstd<-cmplot(df=gre.gpa,xvar=gre.gpa$College.GPA,yvar=gre.gpa$GRE.Q) #Somewhat annoying: this function can only take vector objects for xvar and yvar since it's not operating directly within ggplot
  
  plot.unstd
  
  #We can also customize our plots after-the-fact by calling our ggplot object and adding elements to it. e.g.:
  plot.unstd + geom_point(color="blue")
  
  #Remember the scaled variable dataset we created? Let's make a plot of that and compare.
  plot.std<-cmplot(df=gre.gpa.scaled,xvar=gre.gpa.scaled$College.GPA,yvar=gre.gpa.scaled$GRE.Q)
  plot.std + geom_point(color="red")

  # we can make side-by-side plots using grid.arrange from the gridExtra package
require(gridExtra)
grid.arrange(plot.unstd + geom_point(color="blue") + ggtitle("UNSTANDARDIZED"),
             plot.std + geom_point(color="red") + ggtitle("STANDARDIZED"))
  
#What do you notice about these two plots? What does this illustrate? HINT: we talked about this concept last week. Regression coefficients are invariant over ______.

#now the fun part: let's add a smooting curve. Since we specified a linear model, lets set our method to "lm".
#Let's add a fit line to this using a linear smoothing spline - in other words, adding in our regreCollege.GPAion line:

nicescatter<-cmplot(df=gre.gpa,xvar=gre.gpa$College.GPA,yvar=gre.gpa$GRE.Q) +
  stat_smooth(method = "lm", formula = y ~ x, color = "black", se=T) #smoothing function based on a linear model ("lm"). Here, ggplot is basically doing bivariate regression on the back end. the formula argument here shows the default.

nicescatter


#We can save our plots using the ggsave() function. Note that we can specify the dimensions, and we have multiple options for file types. I recommend pdf because journal copyeditors often prefer it.

#Note that since we set our working directory, this file will save to that location.

ggsave("nicescatter.pdf",nicescatter, width = 7, height = 4, units = "in")

#We haven't talked about quadratic effects yet, but here is how I can change my smoothing function to show a quadratic effect:
cmplot(df=gre.gpa,xvar=gre.gpa$College.GPA,yvar=gre.gpa$GRE.Q) +
  stat_smooth(method = "lm", formula = y~ x + I(x^2), color = "black", se=T)

#One cool thing you can do as well is known as "facet wrapping". This basically means you can do ex

nicescatter + stat_smooth(method = "lm") + 
  # facet_grid(female~.) + #by rows
  # facet_grid(~female) + #by columns, or use facet_wrap:
  facet_wrap(~female)

#If time: let's play around with different specifications of our "female" variable.

gre.gpa$female<-rbinom(n=nrow(gre.gpa),size = 1,prob = .8) #How does my graph change if I change my prob argument? Why?

cmplot(df=gre.gpa,xvar=gre.gpa$College.GPA,yvar=gre.gpa$GRE.Q) + 
  stat_smooth(method = "lm") + 
  # facet_grid(female~.) + #by rows
  # facet_grid(~female) + #by columns, or use facet_wrap:
  facet_wrap(~female)

ggsave(file,values$plot, width = 7, height = 4, units = "in")


#Side note: why do you think the standard error regions are "bow-tie" shaped?