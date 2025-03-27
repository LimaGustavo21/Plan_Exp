table <- matrix(c( 575, 542, 530, 539, 570, 
                        565, 593, 590, 579, 610, 
                        600, 651, 610, 637, 629, 
                        725, 700, 715, 685, 710 ), nrow = 4, ncol = 5, byrow= T )

table

a <- 4

n <- 5


(med_total = mean(table))

#Soma de quadrados Total

(sqtotal = sum((table - med_total)**2))



#Soma de quadrados dos Tratamentos

med_trat <- c()

for (i in 1:a) {
  
  mean <- mean(table[i,])
  med_trat <- c(med_trat,mean)
  
  
}


(sqtrat = n*sum((med_trat - med_total)**2))


#Soma de quadrados dos resíduos

sqres = sqtotal - sqtrat


#Graus de Liberdade 

gl_trat = a - 1

gl_res = (a*n) - a

gl_total = (a*n) - 1 

#Quadrado Médio

qm_trat = sqtrat/gl_trat

qm_res = sqres/gl_res


#Estatística do Teste 

Est_F = qm_trat/qm_res


(pvalor = pf(Est_F,gl_trat,gl_res, lower.tail = F ))




#Construção dos resíduos 

res_total <- matrix(0, 4, 5)

for (i in 1:a) {
  
  for (j in 1:n) {
    
    res <- table[i,j] - (med_total + (med_trat[i] - med_total))
    
    res_total[i,j] <- res
    
    
  }
  
  
}

# Análise de residuo 

res_total

hist(res_total)

#Padronizando os dados


sd_res <- sd(res_total)


res_pad <- res_total/sd_res


#Teste de normalidade shapiro wilk 

shapiro.test(res_total)



#Testes de independência 





  













