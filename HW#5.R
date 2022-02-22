### EDA HW#4 Group

##Load in the SkuMaster.csv dataset. 
setwd("C:\\Users\\User\\Desktop\\Spring 2022\\Exploratory Data Analysis")
dat = read.csv("https://raw.githubusercontent.com/CGarcia44/EDA-/main/6%20SKU%20Master.csv")
dat = na.omit(dat)
str(dat)
dat$ï..SkuNbr = as.numeric(dat$ï..SkuNbr)
dat$Flow = as.factor(dat$Flow)
dat$Whs = as.factor(dat$Whs)
dat$Uom = as.factor(dat$Uom)
dat$Commodity = as.factor(dat$Commodity)
colnames(dat)[colnames(dat)== "ï..SkuNbr"]= "SkuNbr"
dat = dat[dat$UomCube > 0 && dat$UomCube < 2,]
dat = dat[dat$UomWeight < 50 && dat$UomWeight > 0,]
dat = dat[dat$Uom == "CA" | dat$Uom == "EA"|dat$Uom == "PL"|dat$Uom == "LB",]
str(dat$Uom)
dat = na.omit(dat)
dat = droplevels(dat) 


##Create a boxplot on Weight per UOM.  How many observations lie above the upper whisker? Those outliers above the upper whisker are deemed valid and should be left in the dataframe.  
boxplot(dat$UomWeight,
        col="red",
        main="UomWeight",
        sub="Boxplot of UoMWeight",
        cex.sub=2,
        pch=21,
        bg=2,
        cex.main=1.25,
        cex=.75)
        
boxplot.stats(dat$UomWeight)$out

##Create a scatterplot on the Units per Case and the Weight per UOM
plot(dat$UomWeight~dat$Uom,
     type = "p",
     main="UOMWeight on UOM",
     font.main=2,
     ylab="UOMWeight",
     font.label=3,
     xlab="UOM",
     col=rainbow(4),
     cex.sub=2,
     pch=21,
     bg=rainbow(4),
     cex.main=1.25,
     cex=.75,
     legend("topleft",c("CA","EA","LB","PL"),col=c("red","green","cyan","blue"),pch=15))

##Create a plot showing the frequency/count with which the levels of the factor Commodity occur
plot(dat$Commodity,
     main="Commodity Plot",
     font.main=2,
     ylab="Count",
     xlab="Commodity",
     col=c("red","blue","green","yellow","pink"),
     font.axis=1) 

##Create a plot showing the frequency/count with which the levels of the Units of Measure occur
plot(dat$Uom,
     type = "p",
     main="UOM Count",
     font.main=2,
     ylab="Count",
     xlab="UOM",
     pch=1,
     col=rainbow(4),
     ylim=c(0,3000),
     legend("topright",c("CA","EA","LB","PL"),col=c("red","green","cyan","blue"),pch=15,))

##Create a side-by-side boxplot of Cubic Feet by UoM by the types of Flow.  
boxplot(dat$UomCube~dat$Flow,
        type = "p",
        main="UOMCube on Flow",
        font.main=2,
        ylab="UOMCube",
        xlab="Flow",
        pch=1,)

##Create a boxplot on the Weight per UOM.  Which observations/rows appear to be outliers?  
boxplot(dat$UomWeight,
        main="Weight per UOM",
        xlabel="UomWeight",
        ylabel="Count",
        cex.axis=1,
        pch=1,
        col="Cyan")
boxplot.stats(dat$UomWeight)$out

##Create a histogram on Weight per UoM
hist(dat$UomWeight,
     main="Frequency of UOMWeight",
     xlab="UOM Weight",
     ylab="Frequency",
     ylim = c(0,6000),
     col=rainbow(2),
     legend("topright",c("Under 100","Over 100"),col=c("red","cyan"),pch=15))

##Create a dotplot on Weight per Uom labeled with the SKU Number.  Which Sku has the highest Weight per Uom?
dotchart(dat$UomWeight,dat$SkuNbr,
         main="UOMWeight by SKU Number",
         font.main=2,
         font.axis=1,
         xlab="UOMWeight",
         ylab="SKU Number",
         pch=1,
         bg=1,
         cex=.75)

##Create a stripchart for Weight per Uom by the Units of Measure (there should only be Each(EA) and Case(CA)).  
is.factor(dat$Uom)
dat = dat[dat$Uom == "EA" | dat$Uom == "CA",]
stripchart(dat$UomWeight~dat$Uom,
           main="UOMWeight on UOM",
           font.main=2,
           xlab="UOMWeight",
           ylab="UOM",
           pch=21,
           bg=3,
           cex=.75)


