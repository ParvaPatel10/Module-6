# import library
pacman :: p_load(plyr,dplyr,webr,moments,ggplot2,plotrix,magrittr,vcd)
r=getOption("repos")
r["CRAN"]="https://cran.r-project.org/"
options(repos = r)

library(readr)
Titanic <- read_csv("Titanic.csv")
View(Titanic)


# 1. plot of male and female on Titanic
# gender <- Titanic$sex
countgender<-table(Titanic$sex)
lbsgender <- round(countgender/sum(countgender)*100,1)
color<-c("blue","pink")
PieDonut(Titanic, aes(sex)) +
  labs(title = "Ratio of Male and Female on Titanic") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        panel.border = element_blank())

# 2. plot of man, woman and child on Titanic

countwho<-table(Titanic$who)
lbswho <- round(countwho/sum(countwho)*100,1)
color<-c("blue","pink","Light blue")
pie3D(countwho,
      labels = lbswho,
      main = "Ratio of Man, Woman and Child",
      col = color,
)
legend("topright",c("Male","Female","Child"),cex=0.8,fill = c("blue","pink","light blue"))

# 3. plot of Child, Man and Woman in First, Second and Third class

table2<-table(Titanic$class,Titanic$who)
barplot(table2,beside=T,ylim=c(0,500), main = "Ratio of Man, Women and Child in different class")
legend("topright",c("first class","second class","third class"), fill = c("black","Dark grey","Light grey"),cex=0.8)



# 4. plot of people in First, Second and , Third Class

countclass <- table(Titanic$class)
barplot(countclass,main = " People travelling in First, Second and Third Class" , ylim = c(0,700))


# 5. plot of number of people Going to Titanic from which port

hist <- (Titanic$embarked) 
plot(as.factor(hist),ylim = c(0,800), main = "plot of number of people Going to Titanic from which port")


# 6. plot of Child, man and woman going to Titanic from which port 

PieDonut(Titanic,aes(embarked,who),title = " Who entering from which port") 



# 7. plot of Child, Man and women dead and survived in Titanic Crash

table1<- table(Titanic$survived,Titanic$who)
barplot(table1, beside = T,ylim=c(0,500),legend=c("Dead","Survived"),main = "Who dead and survived in Titanic Crash")


# 8. plot of people dead Travelling First, second and Third Class 

PieDonut(Titanic, aes(class, survived), title = "People Survived and dead in different class")


# 9. plot of fair for Child, Man and woman travelling in the Titanic

ggplot(Titanic, aes(x=Titanic$who, y=Titanic$fare)) + 
  geom_boxplot(fill="slateblue") + 
  xlab("fare") +
  ggtitle("fair for Child, Man and woman travelling in the Titanic")


# 10. plot of Child, Man and woman Travelling Alone and Not Alone

Table7 <- table(Titanic$alone, Titanic$who)
barplot(Table7, beside = T,ylim=c(0,500),legend=c("Not Alone","Alone"), main = "Child, Man and woman Travelling Alone or Not Alone")

