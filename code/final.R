# Problem 11.42
df <- read.csv(file = "~/Documents/Stevens/2018FA/MA331/final-project/code/pcb.csv", header = TRUE, sep=",")
summary(df$pcb)
summary(df$pcb52)
summary(df$pcb118)
summary(df$pcb138)
summary(df$pcb180)

boxplot(df$pcb)
boxplot(df$pcb52)
boxplot(df$pcb118)
boxplot(df$pcb138)
boxplot(df$pcb180)

cor(df$pcb, df$pcb52)
cor(df$pcb, df$pcb118)
cor(df$pcb, df$pcb138)
cor(df$pcb, df$pcb180)
cor(df$pcb52, df$pcb118)
cor(df$pcb52, df$pcb138)
cor(df$pcb52, df$pcb180)
cor(df$pcb118, df$pcb138)
cor(df$pcb118, df$pcb180)
cor(df$pcb138, df$pcb180)

plot(df$pcb52, df$pcb, xlab="PCB52", ylab="PCB", main = "PCB vs PCB52")
plot(df$pcb118, df$pcb, xlab="PCB118", ylab="PCB", main = "PCB vs PCB118")
plot(df$pcb138, df$pcb, xlab="PCB138", ylab="PCB", main = "PCB vs PCB138")
plot(df$pcb180, df$pcb, xlab="PCB180", ylab="PCB", main = "PCB vs PCB180")
plot(df$pcb118, df$pcb52, xlab="PCB118", ylab="PCB52", main = "PCB52 vs PCB118")
plot(df$pcb138, df$pcb52, xlab="PCB138", ylab="PCB52", main = "PCB52 vs PCB138")
plot(df$pcb180, df$pcb52, xlab="PCB180", ylab="PCB52", main = "PCB52 vs PCB180")
plot(df$pcb138, df$pcb118, xlab="PCB138", ylab="PCB118", main = "PCB118 vs PCB138")
plot(df$pcb180, df$pcb118, xlab="PCB180", ylab="PCB118", main = "PCB118 vs PCB180")
plot(df$pcb180, df$pcb138, xlab="PCB180", ylab="PCB138", main = "PCB138 vs PCB180")

# Problem 11.43
subdf <- subset(df, select = c("pcb", "pcb52", "pcb118", "pcb138", "pcb180"))
lm1 = lm(pcb~pcb52 + pcb118 + pcb138 + pcb180, data=subdf)
coef(lm1)
summary(lm1)
anova(lm1)

qqnorm(residuals(lm1))
plot(lm1, which=2)

# Problem 11.44
plot(lm1)

df2 <- df[-c(50, 65), ]
subdf2 <- subset(df2, select=c("pcb", "pcb52", "pcb118", "pcb138", "pcb180"))
lm2 = lm(pcb~pcb52+pcb118+pcb138+pcb180, data=subdf2)
coef(lm2)
summary(lm2)
anova(lm2)

# Problem 11.45 
subdf3 <- subset(df, select=c("pcb", "pcb52", "pcb118", "pcb138"))
lm3 = lm(pcb~pcb52+pcb118+pcb138, data=subdf3)
coef(lm3)
summary(lm3)
anova(lm3)

# Problem 11.46
lm4 <- lm(teq~teqpcb+teqdioxin+teqfuran, data=df)
coef(lm4)
summary(lm4)
anova(lm4)

# Problem 11.47
lm5 <- lm(teq~pcb52+pcb118+pcb138+pcb180, data=df)
summary(lm5)
summary(aov(lm5))
plot(lm5, which=1)

# Problem 11.48
df_without_zero <- subset(df, select=c("pcb138", "pcb153", "pcb180", "pcb28", "pcb52", "pcb126", "pcb118", "pcb", "teq", "teqpcb", "teqdioxin", "teqfuran"))
df_without_zero[df_without_zero == 0] <- 0.0026
df_log <- log(df_without_zero)
summary(df_log)
boxplot(df_log)

# Problem 11.49
cor(df_log)
pairs(df_log)

# Problem 11.50
lm6 = lm(pcb~(pcb52+pcb118+pcb138+pcb153+pcb180+pcb28+pcb126), data=df_log)
summary(lm6)
anova(lm6)
plot(lm6, which=c(1,2))

# Problem 11.51
lm7 = lm(teq~(pcb52+pcb118+pcb138+pcb153+pcb180+pcb28+pcb126), data=df_log)
summary(lm7)
anova(lm7)
