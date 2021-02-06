

##################################### PARTE 3: VERIFICAÇÃO DO MODELO AJUSTADO
# foram verificados os modelos 5 e 8 de acordo com os critérios menores de AIC e BIC 

# Teste de estabilidade
autoplot(mod5)
autoplot(mod7)
autoplot(mod8)

# Testes dos resíduos
res5<-residuals(mod5)
res7<-residuals(mod7)
res8<-residuals(mod8)

## Autocorrelação - Teste de Ljung-Box
##H0: os residuos sao iid
tsdiag(mod5)
Box.test(res5,lag=12,type="Ljung-Box")
Box.test(res5,lag=24,type="Ljung-Box")
Box.test(res5,lag=36,type="Ljung-Box")
Box.test(res5,lag=48,type="Ljung-Box")

tsdiag(mod7)
Box.test(res7,lag=12,type="Ljung-Box")
Box.test(res7,lag=24,type="Ljung-Box")
Box.test(res7,lag=36,type="Ljung-Box")
Box.test(res7,lag=48,type="Ljung-Box")

tsdiag(mod8)
Box.test(res8,lag=12,type="Ljung-Box")
Box.test(res8,lag=24,type="Ljung-Box")
Box.test(res8,lag=36,type="Ljung-Box")
Box.test(res8,lag=48,type="Ljung-Box")

## Normalidade                      
##Teste de Jarque-Bera
##H0: normalidade dos residuos
par(mfrow=c(2,2)) ##colocar 2 em cima e 2 embaixo
hist(res5, freq=F, ylab='Densidade', xlab='Resíduos', main='Resíduos')
plot(density(res5, kernel = c("gaussian")), main="Resíduos")   #Função de densidade estimada
qqnorm(res5, ylab='Quantis amostrais', xlab='Quantis teóricos', main='Quantil-Quantil')
qqline(res5, col = "red")
shapiro.test(res5)
jarque.bera.test(res5)

par(mfrow=c(2,2))
hist(res7, freq=F, ylab='Densidade', xlab='Resíduos', main='Resíduos')
plot(density(res8, kernel = c("gaussian")), main="Resíduos")   #Função de densidade estimada
qqnorm(res7, ylab='Quantis amostrais', xlab='Quantis teóricos', main='Quantil-Quantil')
qqline(res7, col = "red")
shapiro.test(res7)
jarque.bera.test(res7)

par(mfrow=c(1,1))
hist(res8, freq=F, ylab='Densidade', xlab='Resíduos', main='Resíduos')
plot(density(res8, kernel = c("gaussian")), main="Resíduos")   #Função de densidade estimada
qqnorm(res8, ylab='Quantis amostrais', xlab='Quantis teóricos', main='Quantil-Quantil')
qqline(res8, col = "red")
shapiro.test(res8)
jarque.bera.test(res8)


# Teste de Heteroscedasticidade     
##Teste ARCH
##H0: os residuos nao possuem efeitos auto-regressivos de heteroscedasticidade condicional
ArchTest(res5,lags = 12)
ArchTest(res7,lags = 12)
ArchTest(res8,lags = 12)
