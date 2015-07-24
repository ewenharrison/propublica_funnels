# ProPublica Surgeon Scorecard 
# https://projects.propublica.org/surgeons

# Laparoscopic cholecystectomy (gallbladder removal) data
# Surgeons with "high adjusted rate of complications"
# CA, NY, TX only

# Libraries needed ----
library(ggplot2)
library(binom)

# Upload dataframe ----
dat = read.csv("ProPublica_CA_NY_TX.csv")

# Total number reported
dim(dat)[1] # 59

# Remove duplicate surgeons who operate in more than one hospital
duplicates = which(
	duplicated(dat$Surgeon)
)

dat_unique = dat[-duplicates,]
dim(dat_unique) # 27

# Funnel plot for gallbladder removal adjusted complication rate -------------------------
# Set up blank funnel plot ----
# Set control limits
pop.rate = 0.044 # Mean population ACR, 4.4%
binom_n = seq(5, 100, length.out=40)
ci.90 = binom.confint(pop.rate*binom_n, binom_n, conf.level = 0.90, methods = "wilson")
ci.95 = binom.confint(pop.rate*binom_n, binom_n, conf.level = 0.95, methods = "wilson")
ci.99 = binom.confint(pop.rate*binom_n, binom_n, conf.level = 0.99, methods = "wilson")

theme_set(theme_bw(24))
g1 = ggplot()+
	geom_line(data=ci.95, aes(ci.95$n, ci.95$lower*100), colour = "blue")+ 
	geom_line(data=ci.95, aes(ci.95$n, ci.95$upper*100), colour = "blue")+
	geom_line(data=ci.99, aes(ci.99$n, ci.99$lower*100), colour = "red")+ 
	geom_line(data=ci.99, aes(ci.99$n, ci.99$upper*100), colour = "red")+
	geom_line(aes(x=ci.90$n, y=pop.rate*100), colour="black", size=1)+
	xlab("Case volume")+
	ylab("Adjusted complication rate (%)")+
	scale_colour_brewer("", type = "qual", palette = 6)+
	theme(legend.justification=c(1,1), legend.position=c(1,1))
g1

g1 + 
	geom_point(data=dat_unique, aes(x=Volume, y=ACR), colour="black", alpha=0.6, size = 6, 
						 show_guide=TRUE)+
	geom_point(data=dat_unique, aes(x=Volume, y=ACR, colour=State), alpha=0.6, size=4) +
	ggtitle("Funnel plot of adjusted complication rate in CA, NY, TX")


# Probability of being shown as having high complication rate ----
# At 4.4%, what are the changes of being 5.2% by chance?
n <- seq(15, 150, 1)
average = 1-pbinom(ceiling(n*0.052), n, 0.044)
low = 1-pbinom(ceiling(n*0.052), n, 0.039)

dat_prob = data.frame(n, average, low)

ggplot(melt(dat_prob, id="n"))+
	geom_point(aes(x=n, y=value*100, colour=variable), size=4)+
	scale_x_continuous("Case volume", breaks=seq(10, 150, 10))+
	ylab("Adjusted complication rate (%)")+
	scale_colour_brewer("True complication\nrate", type="qual", palette = 2, labels=c("Average (4.4%)", "Low (3.9%)"))+
	ggtitle("ProPublica chance of being in high complication rate zone by\nchance when true complication rate \"average\" or \"low\"")