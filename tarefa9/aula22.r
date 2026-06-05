########################################################
#                                                      #
#        Universidade Federal do Rio Grande do Sul     #
#                                                      #
#                 Análise de Sobrevivência             #
#                                                      #
#          Professora Patricia Klarmann Ziegelmann     #
#                                                      #
#                 Modelos Parametricos                 #
########################################################

# Bibliotecas
library(survival)
library(survminer)

# Exemplo Bexiga
dados=data.frame(tempo=c(3,5,6,7,8,9,10,10,12,15,15,18,19,20,22,25,28,30,40,45),
                 evento=c(1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,0))
summary(dados$tempo)
################################################################################
# Estimador de Kaplan Meyer

m1=survfit(Surv(tempo,evento) ~ 1, data=dados, conf.type="log")
m1
summary(m1,extend=TRUE,times=c(10,20,30,40))
ggsurvplot(m1, data=dados,
           fontsize=5,
           title="Reincidência - KM",
           conf.int=T,
           surv.median.line = "h")
resul=data.frame(time=m1$time,KM=m1$surv)
resul
################################################################################
# Modelo Exponencial

# estimativa de maxima verossimilhanca utilizando a formula derivada em aula
sum(dados$tempo)/sum(dados$evento)

m2=survreg(Surv(tempo,evento) ~ 1, data=dados,dist="exponential")
m2
alfa=exp(m2$coefficients[1])
time=seq(0,45,0.001)
s_t=exp(-time/alfa)

plot(time,s_t,type="l",main="Reincidência - Exponencial",col="blue")

# extrapolação no tempo
extrapolacao=data.frame(tempo=c(10,20,30,40,50,60))
extrapolacao$Exponencial=exp(-extrapolacao$tempo/alfa)
extrapolacao

#incluindo os resultados na mesma tabela do KM
resul$Exponencial=exp(-resul$time/alfa)
resul

################################################################################
# Modelo Weibull
m3=survreg(Surv(tempo,evento) ~ 1, data=dados,dist="weibull")
m3
alfa=exp(m3$coefficients[1])
gama=1/m3$scale
time=seq(0,45,0.001)
s_t=exp(-(time/alfa)^gama)
plot(time,s_t,type="l",main="Reinciência - Weibull",col="black")

# extrapolação no tempo
extrapolacao$Weibull=exp(-(extrapolacao$tempo/alfa)^gama)
extrapolacao

#incluindo os resultados na mesma tabela do KM
resul$Weibull=exp(-(resul$time/alfa)^gama)
resul
################################################################################
# Modelo Log_Normal
m4=survreg(Surv(tempo,evento) ~ 1, data=dados,dist="lognorm")
m4
mi=m4$coefficients[1]
sigma=m4$scale
time=seq(0,45,0.001)
s_t=pnorm(-(log(time)-mi)/sigma)
plot(time,s_t,type="l",main="Reinciência - LogNormal",col="magenta")

# extrapolação no tempo
extrapolacao$Log_Normal=pnorm(-(log(extrapolacao$tempo)-mi)/sigma)
extrapolacao

#incluindo os resultados na mesma tabela do KM
resul$Log_Normal=pnorm(-(log(resul$time)-mi)/sigma)
resul