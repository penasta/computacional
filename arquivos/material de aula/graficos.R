## Alguns pacotes
require(ggplot2)
require(gridExtra)
require(ggthemes)
require(dplyr)
require(GGally)
require(vcd)
require(extracat)
require(GDAdata)

## O que � ggplot?
#Data
#Geometric object (geom) pontos? linhas?
#Statistical transformation (stat)
#Scales - linear, log, ...
#Coordinate system
#(+ Position adjustment, facetting)
#layers
qplot(diamonds$carat, diamonds$price)
qplot(carat, price, data = diamonds)
qplot(carat, price, data = diamonds,
      colour=clarity)
qplot(carat, price, data = diamonds,
      geom=c("point", "smooth"))
qplot(carat, data = diamonds,
      geom="histogram")
qplot(carat, data = diamonds,
      geom="histogram", binwidth = 0.25)

## Gr�ficos para dados categorizados

# Spline Plots
(spineplot(margin.table(UCBAdmissions, c(3, 2)), 
           main = "Applications at UCB"))
mosaicplot(margin.table(UCBAdmissions, c(3, 2)),color=T)


# Mosaic Plots
mosaicplot(Titanic, main = "Survival on the Titanic", color = TRUE)
mosaicplot(Titanic, main = "Survival on the Titanic", shade = TRUE)
mosaic(~ Sex + Age + Survived, data = Titanic,
       main = "Survival on the Titanic", shade = TRUE, legend = TRUE)
mosaic(~ Sex + Age + Survived | Class, data = Titanic,
       main = "Survival on the Titanic", shade = TRUE, legend = TRUE)

data(housing, package="MASS")
mosaic(xtabs(Freq ~ Cont + Type + Infl + Sat, data = housing),
       direction = c("h", "v", "v", "h"), 
       gp = gpar(fill = c("grey", "grey","red")),
       spacing = spacing_highlighting)

## Gr�ficos para dados cont�nuos
cars.loc = file.choose()
cars2004 = read.delim(file=cars.loc)
# Scatterplot Matrices
plot(cars2004[,c(4,6,8,9,11)])

require(tourr)
help(package="tourr")
basesFlea = animate(flea[, 1:6], grand_tour(d = 2), display = display_xy())
animate(flea[, 1:6], grand_tour(d = 3), display = display_depth())
animate(flea[, 1:6], grand_tour(d = 4), display = display_pcp())

## parallel coordinates
ggparcoord(iris, columns=1:4, groupColumn="Species")

# interpretando clusters
hcav <- hclust(dist(USArrests), method="ave")
clu3 <- cutree(hcav, k=3)
clus <- factor(clu3)
usa1 <- cbind(USArrests, clus)
ggparcoord(usa1, columns=1:4, groupColumn="clus",
           scale="uniminmax", mapping = aes(size = 1)) +
  xlab("") +  ylab("") +
  theme(legend.position = "none")

pairs(USArrests, panel = panel.smooth, main = "USArrests data")

require(RColorBrewer)
cores = brewer.pal(3,"Accent")
pairs(USArrests, panel = panel.smooth, main = "USArrests data", 
      col=cores[usa1$clus])

## Dados Mistos
require(lattice)
bwplot(City.Miles.Per.Gallon ~ Number.of.Cylinders , data=cars2004,
       horizontal = F)
ggplot(cars2004, aes(factor(Number.of.Cylinders),100/City.Miles.Per.Gallon)) + geom_boxplot() +
  ylab("Gallons per 100 miles") + xlab("Number of Cylinders")

## exemplo de 
## http://www.stat.ubc.ca/~jenny/STAT545A/block09_xyplotLattice.html
gDat <- read.delim("gapminderDataFiveYear.txt")
str(gDat)

## drop Oceania
jDat <- droplevels(subset(gDat, continent != "Oceania"))
str(jDat)
x11()
xyplot(lifeExp ~ gdpPercap | continent, jDat,
       grid = TRUE,
       scales = list(x = list(log = 10, equispaced.log = FALSE)),
       type = c("p", "smooth"), col.line = "darkorange", lwd = 4)
xyplot(lifeExp ~ gdpPercap | continent, jDat,
       grid = TRUE, group = continent,
       scales = list(x = list(log = 10, equispaced.log = FALSE)),
       type = c("p", "smooth"), lwd = 4)
# o primeiro elemento de type pode ser p(ontos), l(inhas), a(mbos); 
# o segundo elemento pode ser r(egress�o) ou smooth.


## Missing Value Plots
# https://cran.r-project.org/web/packages/mi/index.html
require(mi)
data(nlsyV, package = "mi")
mdf <- missing_data.frame(nlsyV)

# verifica se a classe est� correta
show(mdf) 
# momrace is guessed to be ordered
# renda � de fato n�o negativa

mdf <- change(mdf, y = c("income", "momrace"), what = "type",
              to = c("non", "un"))

# tudo certo agora?
show(mdf)

# vamos explorar os dados
summary(mdf)
image(mdf)
hist(mdf)

# esse pacote tamb�m faz imputa��o, que n�o � nosso interesse no momento

## Gr�ficos Conjuntos
x11()
data(Fertility, package="AER")
p0 <- ggplot(Fertility) + geom_histogram(binwidth=1) + ylab("")
p1 <- p0 + aes(x=age)
p2 <- p0 + aes(x=work) + xlab("Weeks worked in 1979")
k <- ggplot(Fertility) + geom_bar() + ylab("") + ylim(0,250000)
p3 <- k + aes(x=morekids) + xlab("has more children")
p4 <- k + aes(x=gender1) + xlab("first child")
p5 <- k + aes(x=gender2) + xlab("second child")
p6 <- k + aes(x=afam) + xlab("African-American")
p7 <- k + aes(x=hispanic) + xlab("Hispanic")
p8 <- k + aes(x=other) + xlab("other race")
grid.arrange(arrangeGrob(p1, p2, ncol=2, widths=c(3,3)),
             arrangeGrob(p3, p4, p5, p6, p7, p8, ncol=6),
             nrow=2, heights=c(1.25,1))