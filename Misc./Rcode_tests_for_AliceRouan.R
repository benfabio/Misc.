
##### Samples of R code to help analyse factors' influence on a quantitative continuous variables © Fabio Benedetti, ETHZ, D-USYS, IBP, UP Group
##### Last update: 11/09/2018

##### Script contains functions aiming to help in:
# 	- generating virtual distributions (normal and non normal) of quantitative variables
# 	- randomly associating qualitative variables to these quantitative variables
# 	- making some basic, but good looking (like me), plots
# 	- performing some basic, but always useful (unlike me), statistical tests
# 	- computing the relative contribution of factors to the variance of a quantitative variable
# 	- performing bootstrap to compute interval confidence/ std.errors for the estimates mentioned right above (#pureswag)
# 	- performing a multivariate analysis to explore the relationships between the factor levels and the quantitative variable
# 	- computing a Gower distance between your observations to estimate their similarity and then, maybe, perform some clustering from the distance matrix
#	- one could also use the observations' coordinates in the multivariate space to perform clustering. I have been doing this for 5 years and it got me a PhD and a postdoc at ETHZ. I'm a fraud.

### Some libraries that might be useful
library("stringr")
library("dplyr")
library("reshape2")
library("tidyverse")
library("FactoMineR")
library("dzidziR")
library("vegan")
library("RColorBrewer")
library("lme4")

##### ============================================================================================================================================

### First, let's simulate fake data to illustrate the case at hand: 1 quantitative continuous variable, and 3-4 factors

# Case 1: you want to simulate data following a normal distribution : rnorm()
# ?rnorm
# https://stats.idre.ucla.edu/r/modules/probabilities-and-distributions/
# http://seankross.com/notes/dpqr/

# Case 2: you want to simulate data following a NON normal distribution (let's say Poisson's): rpois()
# ?rpois
# ?rbeta
hist(rbeta(n = 1000, 5, 5)) # normal
hist(rbeta(1000, 5, 2)) # skewed to the right
hist(rbeta(1000, 2, 5)) # skewed to the left
hist(rbeta(1000, 2, 10))
hist(rbeta(1000, 2, 2)) 
hist(rbeta(1000, 2, 20)) 
mean(rbeta(1000, 2, 20))
# Let's stick to the last case
vec_quanti <- rbeta(1000, 2, 20)
# quanti
summary(vec_quanti)
### Let(s imagine that is a vector of...I don't know, telomere sizes !

### Now, we simulate random factor levels to generate a data.frame that gathers both quanti and factors
# First vector of fictional factors
Fac1 <- LETTERS[1:6] # imagine those are islands or the names of your dummy interns that manipulate samples like they sausages on a BBQ
# Second fictional factor, let's say these represent 3 different parts of the sample, or different samples of the same station
Fac2 <- c("Sample1","Sample2","Sample3")
# Third fictional factor, let's iagine these are traits related to feeding
Fac3 <- c("Active_Ambush_feeder","Current_feeder","Passive_Ambush_feeder","Vegan_biatch","Fero_feeder","Random_feeder","Burger_feeder")
# Fourth one, let's say these are days, or fractal dimensions of coral skeletons (it doesn't really matter at this point)
Fac4 <- c("D1","D2","D3","D4")

# Attribute values of these fictional factors randomly to each quanti measurement
# ?sample
# you may use 'prob' to give some probability in the random sampling
quanti <- data.frame(i = c(1:length(vec_quanti)), quanti = vec_quanti)
quanti$Fac1 <- sample(Fac1, nrow(quanti), replace = TRUE, prob = c(0.1,0.2,0.4,0.05,0.05,0.2) ) 
quanti$Fac2 <- sample(Fac2, nrow(quanti), replace = TRUE, prob = c(0.33,0.33,0.33) ) 
quanti$Fac3 <- sample(Fac3, nrow(quanti), replace = TRUE, prob = c(0.1,0.1,0.1,0.3,0.2,0.1,0.07) ) 
quanti$Fac4 <- sample(Fac4, nrow(quanti), replace = TRUE, prob = c(0.25,0.35,0.1,0.4) ) 
# Check check check
head(quanti) 
summary(quanti)
str(quanti)
# ok

### Before we go right into the testing, let's examine the dataset through some plots
# Boxplots to look at the distribution of quanti between your factors
ggplot(aes(x = Fac1, y = quanti, fill = Fac1), data = quanti) + geom_boxplot() + 
	scale_fill_brewer(palette = "Spectral", name = "") + theme_bw() + xlab("Fac1") + ylab("Quanti (unit)")

ggplot(aes(x = Fac2, y = quanti, fill = Fac2), data = quanti) + geom_boxplot() + 
	scale_fill_brewer(palette = "Spectral", name = "") + theme_bw() + xlab("Fac2") + ylab("Quanti (unit)")

ggplot(aes(x = Fac3, y = quanti, fill = Fac3), data = quanti) + geom_boxplot() + 
	scale_fill_brewer(palette = "Spectral", name = "") + theme_bw() + xlab("Fac3") + ylab("Quanti (unit)")

ggplot(aes(x = Fac4, y = quanti, fill = Fac4), data = quanti) + geom_boxplot() + 
	scale_fill_brewer(palette = "Spectral", name = "") + theme_bw() + xlab("Fac4") + ylab("Quanti (unit)")

### New trick: use notch = TRUE in geom_boxplot() to illustrate the confidence intervals of the medians ! 
ggplot(aes(x = Fac1, y = quanti, fill = Fac1), data = quanti) + geom_boxplot(notch = T) + 
	scale_fill_brewer(palette = "Spectral", name = "") + theme_bw() + xlab("Fac1") + ylab("Quanti (unit)")

# OK, there are no differences in 'quanti' between the levels of each factor but that's kinda obvious considering we generated everything randomly

### Plot the distribution of quanti
ggplot(data = quanti, aes(x = quanti)) +
	geom_histogram(binwidth = 0.01, colour = "black", fill = "grey60") +
	geom_vline(aes(xintercept = median(quanti)), color = "red", linetype = "dashed", size = 1) +
	theme_bw() + xlab("Quanti (unit)")

### Is quanti normally distrbuted ?
shapiro.test(quanti$quanti)
# p-value < 2.2e-16 so we reject the hypothesis that quanti is normally distrbution !
# To check the qqplot
qqnorm(quanti$quanti)
# And if you have qquestions (#blaguedegeek)
# https://stats.stackexchange.com/questions/101274/how-to-interpret-a-qq-plot
### Our qqplot is typical of normal distrib that is skewed to the left.

### You may need to transform your variable a bit to approach a more normal distrbution
# Check the decostand() function and examine the impact of different data transfroamtions
# q
ggplot(data = quanti, aes(x = decostand(quanti, "standardize"))) +
	geom_histogram(colour = "black", fill = "grey60") +
	#geom_vline(aes(xintercept = median(quanti)), color = "red", linetype = "dashed", size = 1) +
	theme_bw() + xlab("Quanti (unit)")

# But we don't really that for now...


### OK, let's start some tests...
# First verify wether the homoscedasticity (homogeneity of variances) is respected or not --> Fligner-Killeen, or Breusch Pagan or Bartlett tests
# This is particularly important if you want to perform variance analyses or linear models (which are similar).
?fligner.test
?bartlett.test
?bptest # of package 'lmtest'
library("lmtest")

### You may tets t for each factor but also for a linear model (which is equivalent to an ANOVA) 
fligner.test(formula = quanti ~ factor(Fac1), data = quanti)
# p-value = 0.1432 -> homoscedasticity is respected
bartlett.test(formula = quanti ~ factor(Fac1), data = quanti)
# p-value = 0.6127 -> homoscedasticity is respected
lmtest::bptest(formula = quanti ~ factor(Fac1), data = quanti)
# p-value = 0.7953 -> homoscedasticity is respected too

# Since we randomly attributed the factors levels it is logic that homoscedasticity is respected for all factors

# And what about a model comprising all factors ?
# https://cran.r-project.org/web/packages/olsrr/vignettes/heteroskedasticity.html
library("olsrr")
# Breusch Pagan Test was introduced by Trevor Breusch and Adrian Pagan in 1979. It is used to test for heteroskedasticity in a linear regression model and assumes that the error terms are normally distributed. It tests whether the variance of the errors from a regression is dependent on the values of the independent variables. It is a χ2test. 
# You can perform the test using the fitted values of the model, the predictors in the model and a subset of the independent variables. It includes options to perform multiple tests and p value adjustments. The options for p value adjustments include Bonferroni, Sidak and Holm’s method.
model <- lm(quanti ~ Fac1 + Fac2 + Fac3 + Fac4, data = quanti)
ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE)
summary(model)


### Now, imagine we want to assess which of these 4 factors generate the most variability in 'quanti'...
model <- lm(quanti ~ Fac1 + Fac2 + Fac3 + Fac4, data = quanti)
# Retrieve the sum of squares of each factor using the anova function, they represent their relative contrib to variance of 'quanti'
ssq <- anova(model)[,2] # retrieve the 'Sum Sq' column, the second one
names(ssq) <- rownames(anova(model)) # associate the factor names to each SSQ
ssq <- t(as.data.frame(ssq)) # 
ssq
# The higher the number, the higher the relative contribution to variance. In our case of random factors distribution, residuals will always be highest because the model cannot be super efficient.

# Furthermore, it would be nice to have some confidence intervals, of standard error, for these SSQ values, to assess the statistical differences between the factors' contribution...the problem is that we have only one value since we have only one data.frame.
# What we do in this case is perform bootstrapping (has nothing to do with actual boots) or jacknife (has nothing to do with actual knives).
# Say you made a simple regression. You wish to know if it is significantly different from zero. In general, people look at the statistic or p.value reported by R. Thing is, this p.value calculation relies on the distribution of your dependent variable. Your software assumes normal distribution if not told differently. It is advisable not to do that, the beauty in bootstrapping is that it is distribution untroubled, it’s valid for dependent which is Gaussian, Cauchy, or whatever. You can use the tool for inference when the underlying distribution is unknown.
# For more info: https://www.statmethods.net/advstats/bootstrapping.html
# ?boot
# boot() calls the statistic function R times. Each time, it generates a set of random indices, with replacement, from the integers 1:nrow(data). These indices are used within the statistic function to select a sample. The statistics are calculated on the sample and the results are accumulated in the 'bootobject''.

# To use boot(), we first need to define a function that will output the 'ssq' object as above. Basically, we take the code above and make a function out of it...
# Important note: the boot requires a functin with at least 2 arguments (x and i, i being the rows/ objects to perform the replication on).
library("boot")
graougraou <- function(x,i) { # 'x' is simply the data containing the data you want to pass on to the lm()
				model <- lm(quanti ~ Fac1 + Fac2 + Fac3 + Fac4, data = x[i,])
				# Retrieve the sum of squares of each factor using the anova function, they represent their relative contrib to variance of 'quanti'
				ssq <- anova(model)[,2] # retrieve the 'Sum Sq' column, the second one
				names(ssq) <- rownames(anova(model)) # associate the factor names to each SSQ
				ssq <- t(as.data.frame(ssq)) # 
				return(ssq) 
} # eo the graougraou function

# Perfom boostrapping on your data to retreive the confidence intervals of the factors' SSQ
booboo <- boot(data = quanti, statistic = graougraou, R = 1000)
# R = number of bootstrap replicates

# The first column ("original") contains the original values from the regular 'ssq' object, so SSQ values without any replication
# What you want is simply the std.error ! The third column...I think these are computed from : "booboo$t"
str(booboo)
dim(booboo$t)
# Put 'booboo$t' in a table and plot the distribution of the SSQ estimates of each factor and residuals sof the linear model
tbl <- data.frame(booboo$t) # 1000 rows since R = 1000
# Provide some colnames
colnames(tbl) <- rownames(anova(model))
head(tbl)
summary(tbl)

# Now, it would be great to plot the variations of SSQ of each factor + the residuals through boxplots. But for that, we first need to melt the tbl
tbl$id <- c(1:nrow(tbl)) # Need to do that to properly melt
mtbl <- melt(tbl, id.var = "id")
colnames(mtbl)[2:3] <- c("Factors","SSQ")
str(mtbl)
# Booya
# Plot
ggplot(aes(x = Factors, y = SSQ, fill = Factors), data = mtbl) + geom_boxplot(notch = T) + 
	scale_fill_brewer(palette = "Spectral", name = "") + theme_bw() + xlab("") + ylab("Sum of squares")
	
# And if you don't like your residuals, then just don't show them ! :p
ggplot(aes(x = Factors, y = SSQ, fill = Factors), data = mtbl[which(mtbl$Factors %in% c("Fac1","Fac2","Fac3","Fac4")),]) + 
	geom_boxplot(notch = T) + scale_fill_brewer(palette = "Spectral", name = "") + theme_bw() + xlab("") + ylab("Sum of squares")

# Depending on the random distribution of factors performed at the beginning of the script, some factors can exhibit way higher contribution to variance than others. And sometimes you won't see any differences.


### Now now now...potential ways to explore the links between 'quanti' and the different factors could be some sort of multivariate ordination analysis
# The problem is that you have 1 quantitative and 4 qualitative variables, so they can't be mixed in a PCA etc. because the analysis will try to estimate their similarity through 'distances', yet the computation of these distances differs between quanti and quali and then even in quanti they can differ depending on the number of zeros and depending if the variable has intergers or decimals and bla bla bla basically my point is : you can't.
# What you CAN do is transform the quantitative variable into a categorical one and then use a multivariate analysis that works on these kind of variables: a Multivariate Correspondance Analysis (MCA) - see Benedetti et al. (2016), J. Plankton Res. aka The Bible.
require("FactoMineR")
# ?MCA
# For more info, refer to Legendre & Legendre (2012) - Numerical Ecology and read : 
# https://fr.wikipedia.org/wiki/Analyse_des_correspondances_multiples
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/

# So, to perform the MCA, we need to make classes out of quanti. Important note : MCA works best when there are even number of levels between the factors and even distrbution inside those levels. For instance, it is better to have 2 factors with 4 levels each than 2 factors with one that presents 3 levels and the other 15 levels.
# Therefore, it would be great to cut 'quanti' into 4/5 levels (aka classes) of equivalent size. For that we may use quantile distribution as follows:
# ?cut
# or : 
library("Hmisc")
# ?cut2
unique( cut2(quanti$quanti, g = 5) ) # voilà ! 
quanti$class <- cut2(quanti$quanti, g = 5)
head(quanti) 
# Check the distributions if you're not convinced !
ggplot(aes(x = class, y = quanti, fill = class), data = quanti) + 
	geom_boxplot(notch = T) + scale_fill_brewer(palette = "Spectral", name = "") + 
	theme_bw() + xlab("")
# Habile. Anyways, let's go back to the MCA right away
str(quanti)
# Hum wait...need to make the factors as factors
quanti$Fac1 <- as.factor(quanti$Fac1)
quanti$Fac2 <- as.factor(quanti$Fac2)
quanti$Fac3 <- as.factor(quanti$Fac3)
quanti$Fac4 <- as.factor(quanti$Fac4)

# Perform MCA - default plots will be returned
mca <- MCA(quanti[,c("Fac1","Fac2","Fac3","Fac4","class")], ncp = 9)

# For a summary: 
summary(mca)
# the first lines indicate the % of explained constrained variance of each MCA axis

# To check the contribution of each variable/factor to variance in the dataset:
mca$var$contrib

# Looking at the eigenvalues (indicators of percentage of explained variance per MCA axis) to choose the nb of signifcant axes (Kaiser-Guttman criterion)
eig <- data.frame(prop = mca$eig[,2], nb = c(1:20))
ggplot(data = eig) + geom_bar(aes(x = nb, y = prop), stat = "identity") + 
	geom_line(aes(x = nb, y = mean(prop) )) + 
	xlab("Eigenvalue Number") +  ylab("Value")
# Basically: keep only the axes whose eigenvalues are higher than the overall mean eigenvalue (the line on the plot)
# Here, keep the first 9 axes (ergo, the number 9 in ncp above).
# NOTE: here, we randomly attributed factors to values so there is no real relationship between 'class' and the factors. As a consequence the MCA has a hard time explaining variance --> many axes and still not a lot of vadriance explained. It won't be like that with 'real' good quality data.
# Remember that what you want in this kind of analysis (PCA/CA/MCA/RDA etc.) is to maximize the variance along the first 2/3 axes because it means the method was efficient at finding patterns in the dataset.
# You now can have fun trying to decypher the relationships between the factors and 'class' on the graph :p
# I could also show how to make a much better plot in ggplot2...humm ok ok lets do it, I can see you are craving it as much as I am craving an IPA beer.

# To find the objects' coordinates in MCA space: str(mca$ind$coord)
# For plotting purposes, let's stick the to the first 4 MCA components only
obj <- data.frame(mca$ind$coord)[1:4]
colnames(obj)[1:4] <- paste("MCA",c(1:4), sep = "")
# cbind() to quanti
quanti <- cbind(quanti, obj)
head(quanti)

# To find the variables' coordinates in MCA space: str(mca$var$coord). Let's put those in another dataframe:
vars <- data.frame(mca$var$coord)[1:4]
colnames(vars)[1:4] <- paste("MCA",c(1:4), sep = "")
vars$v <- rownames(vars)
head(vars)
str(vars)

# So we now may use these two dataframes to plot objects and variables in the same ordination plot/ multivariate reduced space. One may want to retrieve the % of variance of each of the 4 MCA axis to use them as labels of the plot's axes.
mca1 <- paste0("MCA 1 (",floor(eig$prop[1]*100)/100,"%)")
mca2 <- paste0("MCA 2 (",floor(eig$prop[2]*100)/100,"%)")
mca3 <- paste0("MCA 3 (",floor(eig$prop[3]*100)/100,"%)")
mca4 <- paste0("MCA 4 (",floor(eig$prop[4]*100)/100,"%)")
# OK let's ggplot this biatch:
# MCA1 vs MCA2
ggplot() + geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed") +  
		geom_point(aes(x = MCA1, y = MCA2), data = quanti, pch = 21, colour = "black", fill = "#3288bd", alpha = 0.6) +
		geom_point(aes(x = MCA1, y = MCA2), data = vars, pch = 23, colour = "black", fill = "#d53e4f", size = 3) +
		xlab(mca1) + ylab(mca2) + theme_bw()

# MCA3 vs MCA4
ggplot() + geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed") +  
		geom_point(aes(x = MCA3, y = MCA4), data = quanti, pch = 21, colour = "black", fill = "#3288bd", alpha = 0.6) +
		geom_point(aes(x = MCA3, y = MCA4), data = vars, pch = 23, colour = "black", fill = "#d53e4f", size = 3) +
		xlab(mca3) + ylab(mca4) + theme_bw()
		
### One may also play around and directly plot the values of quanti in the MCA space through varying point size, transparency ('alpha'), filling colour...
# Colour
ggplot() + geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed") +  
		geom_point(aes(x = MCA1, y = MCA2, fill = quanti), data = quanti, pch = 21, colour = "black", alpha = 0.8) +
		geom_point(aes(x = MCA1, y = MCA2), data = vars, pch = 23, colour = "black", fill = "#d53e4f", size = 3) +
		xlab(mca1) + ylab(mca2) + theme_bw() + scale_fill_distiller(palette = "YlGnBu", name = "Quanti\n(unit)")

# Transparency
ggplot() + geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed") +  
		geom_point(aes(x = MCA1, y = MCA2, alpha = quanti), data = quanti, pch = 21, colour = "black", fill = "#3288bd") +
		geom_point(aes(x = MCA1, y = MCA2), data = vars, pch = 23, colour = "black", fill = "#d53e4f", size = 3) +
		xlab(mca1) + ylab(mca2) + theme_bw()
		
# Size
ggplot() + geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed") +  
		geom_point(aes(x = MCA1, y = MCA2, size = quanti), data = quanti, pch = 21, colour = "black", fill = "#3288bd", alpha = 0.6) +
		geom_point(aes(x = MCA1, y = MCA2), data = vars, pch = 23, colour = "black", fill = "#d53e4f", size = 3) +
		xlab(mca1) + ylab(mca2) + theme_bw()

# Please note that the same kind of code may be used to re-plot outputs from other multivariate analyses like PCA, CA, RDA etc. because FactoMineR use the same structure for all their resulting objects (e.g. 'mca' in our case).

	
### OK, now how about computing the distances (aka similarity) between the objects ? :)
# https://cran.r-project.org/web/packages/philentropy/vignettes/Distances.html

# We could do it following two ways here:
# - from the 'raw' data in 'quanti', using the quantitative varible and the qualitative variables together. The only distance that accomodates both these types at the same type is Gower distance:
# https://www.rdocumentation.org/packages/StatMatch/versions/1.2.5/topics/gower.dist

# - from the objects' coordinates in the MCA space ! Since these are numerical and decimal, we may use a classic Euclidean distance.

# Let's stick to the first one because it is the one that requires the least data transformation from the actual observations...so it should be the one to compute the most representative similarity between observations. 
library("FD") # There are also the "vegan" and "gower" packages.
?gowdis
gow <- gowdis(x = quanti[,c("quanti","Fac1","Fac2","Fac3","Fac4")]) # simple as that
class(gow)
str(gow)
# You may already plot the Gower distance bewteen objects with a heatmap (nothing to do with the weather) which is the classic representation for distance matrices.
heatmap(as.matrix(gow))

# But this hardly visible (mainly because we have 1000 points). Let's make a nice dendrogram and perform some clustering ! It is fairly simple:
#?hclust
h <- hclust(gow, method = "ward")
plot(h)
# 'method' = the agglomeration method to be used.  This should be (an unambiguous abbreviation of) one of ‘"ward.D"’, ‘"ward.D2"’,‘"single"’, ‘"complete"’, ‘"average"’ (= UPGMA), ‘"mcquitty"’ (= WPGMA), ‘"median"’ (= WPGMC) or ‘"centroid"’ (= UPGMC).
#
# h2 <- hclust(gow, method = "average")
# plot(h2)

# Usually, to make rather large and synoptic groups we use Ward's agglomeration method. But some argued that is the one that creates the most distortions to the initial distance matrix and bla bla...
# Lets cut the dendrogram with the cutree function to retreive 3 clusters (because 3 seems rbobust from the looks of 'h').
cutree(h, 3) # Returns a vector containing the cluster of each of the 1000 obs. SO we can simply pass it to the quanti data.frame !
quanti$cluster <- as.factor(cutree(h, 3))
str(quanti)

# And to interpret the meaning of these clusters, how about some good old boxplots ?
ggplot(aes(x = cluster, y = quanti, fill = cluster), data = quanti) + geom_boxplot(notch = T) + 
	scale_fill_brewer(palette = "Spectral", name = "") + theme_bw() + xlab("") + ylab("Quanti (unit)")
# Not surprisingly, our clusters do not show any difference in the distribution of 'quanti' :p
# But you may also check the distrbution of MCA coordinates etc...
ggplot(aes(x = cluster, y = MCA1, fill = cluster), data = quanti) + geom_boxplot(notch = T) + 
	scale_fill_brewer(palette = "Spectral", name = "") + theme_bw() + xlab("") + ylab("MCA1 scores")
#
ggplot(aes(x = cluster, y = MCA2, fill = cluster), data = quanti) + geom_boxplot(notch = T) + 
	scale_fill_brewer(palette = "Spectral", name = "") + theme_bw() + xlab("") + ylab("MCA2 scores")
# ! But there are clear differences between the clusters in terms of MCA scores which embody combinations of quanti + the associated factors ! :)


### Now, it's YOUR turn: compute an Euclidean distance matrix from the MCA coordinates of the objects (you may use all 9 MCA components or just the first 4), draw a dendrogram and decide on where to cut it to define your clusters ! #somuchfun


### Ideas for potential further analyses : 
# - include some factors as random effects in mixed effects linear models if you want to account for their repetitivity. It is something similar to repeated ANOVAs. 
# https://www.jaredknowles.com/journal/2013/11/25/getting-started-with-mixed-effect-models-in-r

# - generate random distribution (a bit like at the beginning of the script) but for real data to then test how your patterns differ from patterns expected from randomness (allows to further support your findings by showing there are not random, so resulting from actual biological processes)




