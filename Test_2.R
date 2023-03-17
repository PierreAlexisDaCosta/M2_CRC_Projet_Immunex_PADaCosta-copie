library(gtools)
################################################
#For AZ1/AZ4/AZ5/AZ6
#################################################
#Select all density#
MyCovariates<-names(select(survival_data,c("cancer_history", "COPD_history")))


#Cox regression on multiple variable#
covariates <- MyCovariates
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(os_days, status_last_news) ~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = survival_data)})

#Extract data#
univ_results <- 
  lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=2);#exp(beta)
                         LL <- signif(x$conf.int[,"lower .95"], 2)
                         UL <- signif(x$conf.int[,"upper .95"],2)
                         CI <- paste0(HR, " (", 
                                      LL, "-", UL, ")")
                         res<-c(beta, HR, LL,UL,CI,wald.test,p.value)
                         names(res)<-c("beta", "HR","LL","UL","CI", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })

res <- t(as.data.frame(univ_results, check.names = FALSE))
res<-as.data.frame(res)
res<-res[,-c(1,6)]



res<-data.frame(Pheno=rownames(res),res) #Add Phenotypes
res$Pheno<-substring(res$Pheno,11) #Remove first character

res$Pheno<-substring(res$Pheno,1,nchar(res$Pheno)-4) #Remove last character

res$Compartment<-str_sub(res$Pheno, -2)

res$Pheno<-substring(res$Pheno,1,nchar(res$Pheno)-3) #Remove last character
res <- res[order(res$Pheno),] #Reorder by Pheno


#Change p to +, n to - and _ to space
res$Pheno <- gsub('p', '+', res$Pheno)
res$Pheno <- gsub('n', '-', res$Pheno)
res$Pheno <- gsub('_', ' ', res$Pheno)



#Add Index column for forestplot

res<-data.frame(Index=seq(1:nrow(res)),res)



#Add spaces rows for clarity
res$Pheno[seq(from=2,to=nrow(res),by=3)]<-" "
res$Pheno[seq(from=3,to=nrow(res),by=3)]<-" "



#Switch HR LL and UL to numeric for forestplot, Add p value stars and compartment in facotr
res$HR<-as.numeric(res$HR)
res$LL<-as.numeric(res$LL)
res$UL<-as.numeric(res$UL)


res$Stars<-stars.pval(as.numeric(res$p.value))
res$Compartment<-as.factor(res$Compartment)

plot1 <- ggplot(res, aes(y = Index, x = HR,color=Compartment)) +
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0,size=1.2) +
  geom_point(shape = 19, size = 2) +
  geom_vline(xintercept = 1, color = "red", linetype = "solid", cex = 1, alpha = 1) +
  scale_y_continuous(name = "", breaks=1:nrow(res), labels = res$Pheno, trans = "reverse",position="right") +
  scale_x_continuous(trans='log10')+
  xlab("Hazard Ratio (95% Confidence Interval)") + 
  ylab("") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line.x = element_line(colour = "black", linewidth =1),
        axis.line.y=element_blank(),
        axis.text.y = element_text(size = 12, colour = "black",face="bold",hjust = 0),
        axis.text.x.bottom = element_text(size = 12, colour = "black",face="bold"),
        axis.title.x = element_text(size = 12, colour = "black",face="bold"),
        axis.ticks.y=element_blank())+
  geom_hline(yintercept = seq(0.5, nrow(res)+0.5,by=3), color = "grey", linetype = "solid", cex = 0.5, alpha = 1)+
  theme(plot.margin=unit(c(0,0,0,0), 'cm'))
plot1



## Create the table-base pallete
table_base <- ggplot(res, aes(y=Pheno)) +
  ylab(NULL) + xlab("  ") + 
  theme(plot.title = element_text(hjust = 0.5, size=12), 
        axis.text.x = element_text(color="white", hjust = -3, size = 25), ## This is used to help with alignment
        axis.line = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_blank())

## p value table
tab3 <- table_base +
  geom_text(aes(y = rev(Index), x = 1, label = p.value), size = 2.8,fontface="bold") + 
  
  theme(plot.title = element_text(face = "bold"),
        panel.border = element_blank())+
  geom_hline(yintercept = seq(0.5, nrow(res)+0.5,by=3), color = "grey", linetype = "solid", cex = 0.5, alpha = 1)+ #Add as much lines as necessary#
  theme(plot.margin=unit(c(0,0,0,0), 'cm'))
## 95% CI table
tab2 <- table_base +
  geom_text(aes(y = rev(Index), x = 1, label = CI), size = 2.8,fontface="bold") + 
  theme(plot.title = element_text(face = "bold"))+
  geom_hline(yintercept = seq(0.5, nrow(res)+0.5,by=3), color = "grey", linetype = "solid", cex = 0.5, alpha = 1)+ #Add as much lines as necessary#
  theme(plot.margin=unit(c(0,0,0,0), 'cm'))

## Stars p value table
tab4 <- table_base +
  geom_text(aes(y = rev(Index), x = 1, label = Stars), size = 3,fontface="bold") + 
  theme(plot.title = element_text(face = "bold"))+
  geom_hline(yintercept = seq(0.5, nrow(res)+0.5,by=3), color = "grey", linetype = "solid", cex = 0.5, alpha = 1)+ #Add as much lines as necessary#
  theme(plot.margin=unit(c(0,0,0,0), 'cm'))

lay <-  matrix(c(1,2,3,3,3,3,3,3,4), nrow = 1)
grid.arrange(tab4,tab3, plot1, tab2, layout_matrix = lay)


plot_grid(tab3,tab2,tab4, plot1, align = "h", nrow = 1, rel_widths = c(1/16,3/16, 1/16, 11/16))




