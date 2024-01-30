### Logistic regression with one categorical explanatory variable

\textcolor{red}{1.AgeGroup vs obese}

```{r echo=FALSE}
model_age<-glm(BMIgroup ~ AgeGroup, data = lifestyle , family  = binomial(link = "logit"))

model_coef = model_age %>%
  summary() %>%
  coef()

kable(model_coef,caption='\\label{tab:coef1}Estimates of the intercept and slope from the model.',
      booktabs=TRUE,digits=2)%>%
  kable_styling(font_size=10,latex_options="hold_position")
```


```{r echo=FALSE,results='hide',fig.keep='all',out.width = '68%',fig.align = "center",fig.cap = "\\label{fig:age1} Predicted probabilities of obesity",fig.pos = 'H'}
plot_model(model_age,type = "pred",axis.title = c("AgeGroup","Probability of obesity"),title=" ")
```

\textcolor{red}{2.Sex vs obese}

```{r echo=FALSE}
model_sex<-glm(BMIgroup ~ Sex, data = lifestyle , family  = binomial(link = "logit"))

model_coef = model_sex %>%
  summary() %>%
  coef()

kable(model_coef,caption='\\label{tab:coef2}Estimates of the intercept and slope from the model.',
      booktabs=TRUE,digits=2)%>%
  kable_styling(font_size=10,latex_options="hold_position")
```

```{r echo=FALSE,results='hide',fig.keep='all',out.width = '68%',fig.align = "center",fig.cap = "\\label{fig:sex1} Predicted probabilities of obesity",fig.pos = 'H'}
plot_model(model_sex,type = "pred",axis.title = c("Sex","Probability of obesity"),title=" ")
```



\textcolor{red}{3.Employment vs obese}

```{r echo=FALSE}
model_employment<-glm(BMIgroup ~ Employment, data = lifestyle , family  = binomial(link = "logit"))

model_coef = model_employment %>%
  summary() %>%
  coef()

kable(model_coef,caption='\\label{tab:coef3}Estimates of the intercept and slope from the model.',
      booktabs=TRUE,digits=2)%>%
  kable_styling(font_size=10,latex_options="hold_position")
```

```{r echo=FALSE,results='hide',fig.keep='all',out.width = '68%',fig.align = "center",fig.cap = "\\label{fig:employment1} Predicted probabilities of obesity",fig.pos = 'H'}

lifestyle1<-lifestyle
lifestyle1$Employment<-as.factor(lifestyle1$Employment)
levels(lifestyle1$Employment) <- c("something else","education","employed","home","look for","unable","retired")

model_employment1<-glm(BMIgroup ~ Employment, data = lifestyle1 , family  = binomial(link = "logit"))
plot_model(model_employment1,type = "pred",title=" ",axis.title = c("Employment","Probability of obesity"))

```