######################################
## Tyvan Virtues Free-List
######################################

# Code written by Benjamin Grant Purzycki and Theiss Bendixen
# Contact email: bgpurzycki@cas.au.dk
# Last Updated May 25, 2020 by BGP

rm(list = ls())

### Set-up (need to install before using)
library(AnthroTools) # https://anthrotools.wordpress.com/materials/
library(xtable)
library(rethinking) # https://github.com/rmcelreath/rethinking

setwd("") # set working directory

d <- read.delim("demo.txt") # load demographic data
FL10 <- read.delim("FL10_Virtues.txt") # load free-list data

demfun <- function(x) { # summary stats function
  mean <- mean(x, na.rm = TRUE)
  sd <- sd(x, na.rm = TRUE)
  median <- median(x, na.rm = TRUE)
  min <- min(x, na.rm = TRUE)
  max <- max(x, na.rm = TRUE)
  N <- sum(table(x))
  return((data.frame(M = mean, SD = sd, 
                     med = median, min = min, 
                     max = max, N = N)))
}

d$urbprop <- d$HOWLONGCITY.2/d$Age # prop. of urban life
d$urbprop100 <- d$urbprop*100 # * 100
d$urbprop100.c <- d$urbprop100 - mean(d$urbprop100, na.rm = T) # center urbprop
d$Age.c <- d$Age - mean(d$Age, na.rm = T) # center age

### Participant Demographics
demo <- data.frame(d$Age, d$CHILDREN, d$FORMALED.2, d$HOWLONGCITY.2)
demo <- setNames(demo, c("AGE","CHILDREN","FORMALED", "CITYYRS"))
tab <- t(sapply(demo, demfun))
xtable(tab)

as.data.frame(table(cut(d$urbprop, breaks = seq(0, 1, by = 0.5))))

### Urbanity figure
par(mar = c(5, 5, 2, 5))
hist(d$urbprop, breaks = 10, prob = F, xlim = c(-.2, 1.2),
     main = NA, xlab = "% of life spent living in urban environment")
par(new = TRUE)
plot(density(d$urbprop, na.rm = T, adjust = .7), lwd = 2, type = "l",
     main = NA, ylab = NA, xlab = NA, axes = F)
abline(v = 0.5, lty = 2, lwd = 2)
axis(side = 4, at = pretty(range(d$urbprop, na.rm = T)))
mtext(side = 4, "Density", line = 3, adj = .3)

##############################
### Free-List Data Analyses

# Salience analysis on Group
FL.G <- CalculateSalience(FL10, Subj = "Subj", Order = "Order", CODE = "GC",
                          Salience = "GC.S")
FL.S <- CalculateSalience(FL.G, Subj = "Subj", Order = "Order", CODE = "BC",
                          Salience = "BC.S")
GFL.S <- SalienceByCode(FL.S, Subj = "Subj", CODE = "GC", Salience = "GC.S",
                        dealWithDoubles = "MAX")
BFL.S <- SalienceByCode(FL.S, Subj = "Subj", CODE = "BC", Salience = "BC.S",
                        dealWithDoubles = "MAX")

# Chechek Plots
par(mfrow = c(1,2), mar = c(.02, .02, .02, .02))
AnthroTools:::FlowerPlot(GFL.S, "Good")
AnthroTools:::FlowerPlot(BFL.S, "Bad")

# Salience analysis by groups
# Merge FL with sex 
names(d)[names(d) == "d"] <- "Subj"
FL.SEX <- merge(x = FL10, y = d[,c("Subj","Sex")], by = "Subj")

# Item salience
Sex.FL.G <- CalculateSalience(FL.SEX, Order = "Order", Subj = "Subj", 
                              CODE = "GC", # Calculate item salience, Good
                              GROUPING = "Sex", Rescale = FALSE, 
                              Salience = "GC.Sex")

Sex.FL.B <- CalculateSalience(Sex.FL.G, Order = "Order", Subj = "Subj", 
                              CODE = "BC",  # Calculate item salience, Bad
                              GROUPING = "Sex", Rescale = FALSE, 
                              Salience = "BC.Sex")

# Smith's S
Sex.GOOD.FL.S <- SalienceByCode(Sex.FL.G, Subj = "Subj", 
                              CODE = "GC", GROUPING = "Sex", # Calculate Smith's S, Good
                              Salience = "GC.Sex", dealWithDoubles = "MAX")
Sex.BAD.FL.S <- SalienceByCode(Sex.FL.B, Subj = "Subj", CODE = "BC", 
                              GROUPING = "Sex", # Calculate Smith's S, Bad
                              Salience = "BC.Sex", dealWithDoubles = "MAX")

# Item Salience
Wom.GOOD.F <- subset(Sex.GOOD.FL.S, GROUPING == "0")
Wom.BAD.F <-  subset(Sex.BAD.FL.S, GROUPING == "0")
Wom.GOOD.F$GROUPING <- NULL # delete group var for plots
Wom.BAD.F$GROUPING <- NULL # delete group var for plots

Men.GOOD.F <- subset(Sex.GOOD.FL.S, GROUPING == "1")
Men.BAD.F <-  subset(Sex.BAD.FL.S, GROUPING == "1")
Men.GOOD.F$GROUPING <- NULL # delete group var for plots 
Men.BAD.F$GROUPING <- NULL # delete group var for plots

# Plotting women vs. men
par(mfrow = c(2,2), mar = c(.00, .01, .00, .01))
AnthroTools:::FlowerPlot(Wom.GOOD.F, "Good (F)")
AnthroTools:::FlowerPlot(Wom.BAD.F, "Bad (F)")
AnthroTools:::FlowerPlot(Men.GOOD.F, "Good (M)")
AnthroTools:::FlowerPlot(Men.BAD.F, "Bad (M)")

### Predicting alcohol as bad
FL.bin <- FreeListTable(FL.S, Order = "Order", CODE = "BC", 
                        tableType = "PRESENCE")
table(FL.bin$alcohol)
names(FL.bin)[names(FL.bin) == "Subject"] <- "Subj"
aa <- merge(x = FL.bin, 
            y = d[,c("Subj","Age.c", "Sex", "CHILDREN", "urbprop100.c")], 
            by = "Subj")
labs <- c("Subj", "alcohol", "Sex", "urbprop100.c", "Age.c", "CHILDREN")
dat <- aa[labs] # subset
aaa <- dat[complete.cases(dat), ] # need complete cases from focal vars

## Describe frequencies
FL.B <- FL.bin
FL.B$Subj <- NULL
FL.B$sum <- rowSums(FL.B)
FL.B <- FL.B[!(FL.B$sum == 0),]
demfun(FL.B$sum) # listed in bad list

FL.B2 <- FreeListTable(FL.S, Order = "Order", CODE = "GC", 
                        tableType = "PRESENCE")
FL.B2$Subject <- NULL
FL.B2$sum <- rowSums(FL.B2)
FL.B2 <- FL.B2[!(FL.B2$sum == 0),]
demfun(FL.B2$sum) # listed in good list

# Set up variables for regression
y <- aaa$alcohol 
sex <- as.numeric(aaa$Sex)
age <- aaa$Age.c
urban <- aaa$urbprop100.c
children <- aaa$CHILDREN

dat_list <- list( # bind them in a list format
  y = y,
  sex = sex,
  age = age,
  urban = urban,
  children = children
  )

alcmod <- map( # model (might have to run twice)
  alist(
    y ~ dbinom(1, p),
    logit(p) <- a + bs*sex + ba*age + bu*urban + bc*children,
    a ~ dnorm(0, 10),
    c(bs, ba, bu, bc) ~ dnorm(0, 1)
  ), 
  data = dat_list)
(precmod <- precis(alcmod, depth = 2, prob = .95))

# Table
xtable(precmod)

# OR Plot
exp(precmod)
labs <- c("Intercept", "Sex (male = 1)", "Age*", "Urbanity*", "Children")
x <- 1:5
OR <- c(2.22, 0.34, 1.02, 1.01, 0.95)
LL <- c(0.86, 0.14, 0.98, 0.99, 0.63)
UL <- c(5.73, 0.83, 1.07, 1.03, 1.42)
LS <- OR - LL
US <- UL - OR

par(mfrow = c(1, 1), mar = c(2, 7, 1, 1))
plot(OR, x, pch = 16, xlim = c(.05, 7), 
     ylim = c(0.5, 5.2), xlab = "Odds Ratio", 
     ylab = NA, yaxt = "n", frame.plot = F, log = "x")
arrows(x0 = OR - LS, y0 = x, 
       x1 = US + OR, y1 = x, 
       code = 3, angle = 90, length = 0.07)
abline(v = 1, lty = 2)
axis(2, at = x, labels = labs, las = 2)

# Family and traditions salience
sallabs1 <- c("Subj", "Order", "GC1") # GC1: traditions
salG1 <- FL.S[sallabs1]
salG1 <- salG1[complete.cases(salG1), ] # need complete cases from focal vars
salG1.S <- CalculateSalience(salG1, Order = "Order", 
                    Subj = "Subj", CODE = "GC1",
                    Salience = "GC1.S")
maxsalG1 <- FreeListTable(salG1.S, CODE = "GC1", Order = "Order",
                    Salience = "GC1.S",
                    Subj = "Subj", tableType = "MAX_SALIENCE")
names(maxsalG1)[names(maxsalG1) == "Subject"] <- "Subj"
Gsal1 <- merge(x = maxsalG1, y = d[,c("Subj","Age", "Age.c")], by = "Subj")

sallabs2 <- c("Subj", "Order", "GC2") # GC2: traditions/family
salG2 <- FL.S[sallabs2]
salG2 <- salG2[complete.cases(salG2), ] # need complete cases from focal vars
salG2.S <- CalculateSalience(salG2, Order = "Order", 
                    Subj = "Subj", CODE = "GC2",
                    Salience = "GC2.S")
maxsalG2 <- FreeListTable(salG2.S, CODE = "GC2", Order = "Order", 
                    Salience = "GC2.S",
                    Subj = "Subj", tableType = "MAX_SALIENCE")
names(maxsalG2)[names(maxsalG2) == "Subject"] <- "Subj"
Gsal2 <- merge(x = maxsalG2, y = d[,c("Subj","Age", "Age.c")], by = "Subj")

# Regressions
(mg1 <- lm(traditions*100 ~ Age.c, data = Gsal1))
(mg2 <- lm(`traditions/family`*100 ~ Age.c, data = Gsal2))
(mi <- lm(intelligent*100 ~ Age.c, data = Gsal1))
(mh <- lm(`hard-working`*100 ~ Age.c, data = Gsal1))

confint(mg1)
confint(mg2)
confint(mi)
confint(mh)

coef(mg2)[1] + coef(mg2)[2]*30

# Family and traditions salience plot
par(mfrow = c(2, 2), mar = c(2, 4.3, 1, 2), oma = c(2, 0, 0, 0))
#b,l,t,r
plot(jitter(traditions*100, factor = 3) ~ jitter(Age.c, factor = 3), 
     pch = 16,
     xlab = NA, 
     ylab = NA,
     data = Gsal1)
newx1 <- seq(min(Gsal1$Age.c, na.rm = T), 
             max(Gsal1$Age.c, na.rm = T), length.out = 100)
preds1 <- predict(mg1, newdata = data.frame(Age.c = newx1), 
                 interval = 'confidence')
polygon(c(rev(newx1), newx1), 
        c(rev(preds1[ ,3]), 
          preds1[ ,2]), col = rgb(0, 0, 0, 0.3), border = NA)
abline(mg1)
mtext(padj = -2.5, side = 2, expression(italic('s'[i])*' of tradition'))
text(30, 95, "(a)", cex = 1.3)

plot(jitter(`traditions/family`*100, factor = 3) ~ jitter(Age.c, factor = 3), 
     pch = 16,
     xlab = NA, 
     ylab = NA,
     data = Gsal2)
newx2 <- seq(min(Gsal2$Age.c, na.rm = T), 
            max(Gsal2$Age.c, na.rm = T), length.out = 100)
preds2 <- predict(mg2, newdata = data.frame(Age.c = newx2), 
                 interval = 'confidence')
polygon(c(rev(newx2), newx2), 
        c(rev(preds2[ ,3]), 
          preds2[ ,2]), col = rgb(0, 0, 0, 0.3), border = NA)
abline(mg2)
mtext(padj = -2.5, side = 2, expression(italic('s'[i])*' of tradition/family'))
text(30, 95, "(b)", cex = 1.3)

plot(jitter(intelligent*100, factor = 3) ~ jitter(Age.c, factor = 3), data = Gsal1, pch = 16,
     ylab = NA,
     xlab = NA)
newx3 <- seq(min(Gsal1$Age.c, na.rm = T), 
            max(Gsal1$Age.c, na.rm = T), length.out = 100)
preds3 <- predict(mi, newdata = data.frame(Age.c = newx3), 
                 interval = 'confidence')
polygon(c(rev(newx3), newx3), 
        c(rev(preds3[ ,3]), 
          preds3[ ,2]), col = rgb(0, 0, 0, 0.3), border = NA)
abline(mi)
mtext(side = 1, padj = 3.3, "Age (centered)")
mtext(padj = -2.5, side = 2, expression(italic('s'[i])*' of intelligent'))
text(30, 95, "(c)", cex = 1.3)

plot(jitter(`hard-working`*100, factor = 3) ~ jitter(Age.c, factor = 3), 
     pch = 16,
     ylab = NA,
     xlab = NA, data = Gsal1)
newx4 <- seq(min(Gsal1$Age.c, na.rm = T), 
            max(Gsal1$Age.c, na.rm = T), length.out = 100)
preds4 <- predict(mh, newdata = data.frame(Age.c = newx4), 
                 interval = 'confidence')
polygon(c(rev(newx4), newx4), 
        c(rev(preds4[ ,3]), 
          preds4[ ,2]), col = rgb(0, 0, 0, 0.3), border = NA)
abline(mh)
mtext(side = 1, padj = 3.3, "Age (centered)")
mtext(padj = -2.5, side = 2, expression(italic('s'[i])*' of hard-working'))
text(30, 85, "(d)", cex = 1.3)

