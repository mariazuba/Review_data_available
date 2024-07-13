## Prepare plots and tables for report

## Before:
## After:

library(icesTAF)
library(flextable)

mkdir("report")

# Load data ---------------------------------------------------------------

# input data
load("data/input.RData") 
load("output/inputData.RData") 

#Figures ----

# surveys indices (biomass) ----
fig1<-indices_a %>% ggplot(aes(x=year, y=weight, color = factor(survey))) +
  geom_line()+geom_point()+
  #facet_grid(year~.,  as.table = TRUE)+
  #facet_wrap(vars(year), dir = 'v', as.table = TRUE) +
  labs(x = 'Years', y = 'Biomass (ton)') +
   scale_colour_discrete(name  ="")+
  # scale_colour_manual(values=c('red','green','blue','magenta')) +
  theme(panel.background = element_rect(fill ="gray99")) +
  theme(panel.grid=element_line(color=NA)) +
  ggtitle("")+
  theme(plot.title = element_text(size = 4),legend.position="top")
ggsave("report/InputData.png", fig1,  width=8, height=4)


# surveys indices (abundance) ----
fig2<-adist_surveys %>% ggplot(aes(x=year, y=round(number/1000,2),fill=factor(age))) + 
  geom_line()+geom_bar(stat="identity")+
  facet_wrap(vars(survey), dir = 'v', as.table = TRUE) +
  labs(x = 'Age', y = 'Numbers x10^3') +
  scale_fill_discrete(name  ="Age")+
  theme(panel.background = element_rect(fill ="gray99")) +
  theme(panel.grid=element_line(color=NA)) +
  ggtitle('')+
  theme(plot.title = element_text(size = 12),
        panel.background = element_rect(fill = "gray99"),
        strip.background = element_rect(colour = "white", fill = "gray89")) 
ggsave("report/ageComp_Surveys.png", fig2,  width=4, height=8)



## Length composition ----
### PELAGO
fig3<-ggplot(ldist_pelago, 
      aes(x=as.numeric(length), y=round(number/1000,2))) + 
  geom_bar(stat="identity",fill="gray75")+
  facet_grid(year+step~.,  as.table = TRUE,scale="free") +
  scale_x_continuous(breaks=seq(0,25,2.5), limits=c(3, 20))+
  labs(x = 'Length (cm)', y = 'Numbers x 10^3') +
  theme(panel.background = element_rect(fill ="gray99")) +
  theme(panel.grid=element_line(color=NA)) +
  ggtitle('PELAGO')+
  theme(plot.title = element_text(size = 12),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 6),
        strip.text = element_text(size = 6),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(colour = "white", fill = "white")) + theme(legend.position = 'top')
ggsave("report/lengthComp_Pelago.png", fig3,  width=4, height=8)

## Age composition ----
### PELAGO ----
fig4<-ggplot(adist_pelago, 
       aes(x=as.numeric(age), y=round(number/1000,2),fill=factor(age))) + 
  geom_bar(stat="identity")+
  facet_wrap(~year+step,  ncol=2, as.table = TRUE,scale="free",strip.position = "right") +
  labs(x = 'Age', y = 'Numbers x 10^3') +
  scale_fill_discrete(name  ="Age")+
  theme(panel.background = element_rect(fill ="gray99")) +
  theme(panel.grid=element_line(color=NA)) +
  ggtitle('PELAGO')+
  theme(plot.title = element_text(size =12),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 6),
        strip.text = element_text(size = 6),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(colour = "white", fill = "white")) + 
  theme(legend.position = 'top') 
ggsave("report/ageComp_Pelago.png", fig4,  width=7, height=8)
### ECOCADIZ ----
fig5<-ggplot(adist_ecocadiz, 
             aes(x=as.numeric(age), y=round(number/1000,2),fill=factor(age))) + 
  geom_bar(stat="identity")+
  facet_wrap(~year+step,  ncol=2, as.table = TRUE,scale="free",strip.position = "right") +
  labs(x = 'Age', y = 'Numbers x 10^3') +
  scale_fill_discrete(name  ="Age")+
  theme(panel.background = element_rect(fill ="gray99")) +
  theme(panel.grid=element_line(color=NA)) +
  ggtitle('ECOCADIZ')+
  theme(plot.title = element_text(size =12),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 6),
        strip.text = element_text(size = 6),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(colour = "white", fill = "white")) + 
  theme(legend.position = 'top') 
ggsave("report/ageComp_Ecocadiz.png", fig5,  width=7, height=8)
### ECOCADIZ-RECLUTAS ----
fig6<-ggplot(adist_ecocadizR, 
             aes(x=as.numeric(age), y=round(number/1000,2),fill=factor(age))) + 
  geom_bar(stat="identity")+
  facet_wrap(~year+step,  ncol=2, as.table = TRUE,scale="free",strip.position = "right") +
  labs(x = 'Age', y = 'Numbers x 10^3') +
  scale_fill_discrete(name  ="Age")+
  theme(panel.background = element_rect(fill ="gray99")) +
  theme(panel.grid=element_line(color=NA)) +
  ggtitle('ECOCADIZ-RECLUTAS')+
  theme(plot.title = element_text(size =12),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 6),
        strip.text = element_text(size = 6),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(colour = "white", fill = "white")) + 
  theme(legend.position = 'top') 
ggsave("report/ageComp_EcocadizRecl.png", fig6,  width=7, height=8)


## Weight-at-age
wage_seine$weight[wage_seine$weight==0]<-NA
fig7<-wage_seine %>% ggplot(aes(x=as.numeric(year),y=weight,colour=age)) +
  geom_point() + geom_line()+
  facet_wrap(.~step,ncol=2,as.table = TRUE, strip.position = "top")+
  labs(x="Year",y="Weight mean (grs)")+
  scale_color_discrete(name  ="Age")+
  theme(panel.background = element_rect(fill ="gray80")) +
  theme(panel.grid=element_line(color=NA)) +
  ggtitle('SEINE')+
  theme(plot.title = element_text(size =12),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 6),
        strip.text = element_text(size = 6),
        panel.background = element_rect(colour="gray",fill = "gray99"),
        strip.background = element_rect(colour = "gray", fill = "gray99")) + 
  theme(legend.position = 'top') 
ggsave("report/Wage_quarters.png", fig7,  width=7, height=8)

## Length-at-age
Lage_seine$weight[Lage_seine$weight==0]<-NA
fig8<-Lage_seine %>% ggplot(aes(x=as.numeric(year),y=weight,colour=age)) +
  geom_point() + geom_line()+
  facet_wrap(.~step,ncol=2,as.table = TRUE, strip.position = "top")+
  labs(x="Year",y="Length mean (cm)")+
  scale_color_discrete(name  ="Age")+
  theme(panel.background = element_rect(fill ="gray80")) +
  theme(panel.grid=element_line(color=NA)) +
  ggtitle('SEINE')+
  theme(plot.title = element_text(size =12),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 6),
        strip.text = element_text(size = 6),
        panel.background = element_rect(colour="gray",fill = "gray99"),
        strip.background = element_rect(colour = "gray", fill = "gray99")) + 
  theme(legend.position = 'top') 
ggsave("report/Lage_quarters.png", fig8,  width=7, height=8)




#Tables ----
ft0<-flextable(info_survey)
ft0 <- set_header_labels(ft0, 
                         Year="year",
                         ECOCADIZ="ECOCADIZ",
                         ECOCADIZRECLUTAS="ECOCADIZ-RECLUTAS",
                         PELAGO="PELAGO")
ft0 <- add_header_row(ft0, 
                      values = c("", "Acoustic survey"),
                      colwidths = c(1, 3))
ft0 <- colformat_double(ft0, digits=0, na_str = "")
ft0 <- colformat_num(ft0,big.mark = "", na_str = "")
ft0 <- align(ft0,part = "header", align = "center") 
ft0 <- autofit(ft0)
ft0


ft1<-flextable(round(indices,0))
ft1 <- set_header_labels(ft1, 
                         Year="year",
                         ECOCADIZ="ECOCADIZ",
                         ECOCADIZRECLUTAS="ECOCADIZ-RECLUTAS",
                         PELAGO="PELAGO")
ft1 <- add_header_row(ft1, 
                      values = c("", "Acoustic survey (ton)"),
                      colwidths = c(1, 3))
# ft1 <- add_header_row(ft1, 
#                       values = c("", "ECOCADIZ", "ECOCADIZ-RECLUTAS", "PELAGO"),
#                       colwidths = c(1, 1, 1,1))

ft1 <- colformat_double(ft1, digits=0, na_str = "")
ft1 <- colformat_num(ft1,big.mark = "", na_str = "")
ft1 <- align(ft1,part = "header", align = "center") 
ft1 <- autofit(ft1)
ft1

save(ft0,ft1, file="report/tables_run.RData")
