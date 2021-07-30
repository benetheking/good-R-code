########################################
###########
## Install packages list
install.packages("manipulateWidget")
install.packages("shinydashboard")
install.packages("tidymodelling")
install.packages("shinyWidgets")
install.packages("data.table")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggthemes")
install.packages("devtools")
install.packages("hexView")
install.packages("prophet")
install.packages("plotly")
install.packages("tcltk2")
install.packages("lmtest")
install.packages("Hmisc")
install.packages("rlang")
install.packages("shiny")
install.packages("broom")
install.packages("xlsx")
install.packages("onls")
install.packages("Rcpp")
install.packages("rio")
install.packages("xts")
install.packages("car")
install.packages("pak")

## REG in R
datis <- full_data %>%
  select(log_ELV_APO_GES70, av, rep(1, 157) )

plot(datis)
lines(predict(lm(av~., datis)))
(lm(y~0+V1, dat2))
x <- matrix(c(datis[,1], rep(1, 157) ), ncol=2 )
x <- matrix(datis[,1], ncol=1 )
y <- av

dat2 <- cbind(x, y) %>% as.data.frame()

beta_hat <- ((t(x)%*%x)^-1)%*%(t(x)%*%y)

#### LOG REG
library(tidyverse)
x_1 <- sample(0:40, 30, T)
y_1 <- ifelse(x_1 > 15, 1, 0)
## add some noise
x_1 <- x_1+rnorm(30, mean=5)
daten <- data.frame(y_1, x_1)

modl <- glm(y_1 ~ 0+ x_1, data=daten, family = "binomial")
summary(modl)
daten$preds <- predict(modl, type="response")
predict(modl)
plot(x=daten$x_1, y=daten$y_1)
lines(x=daten$x_1, y=daten$preds)
plot(x=daten$x_1, y=daten$preds)

