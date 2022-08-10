# Bar charts describing the scoping review results

# Set-up ----

library(ggplot2)
windowsFonts(font = windowsFont("Times New Roman"))


# Rise of multiverse analysis studies ----

df <- data.frame(n_papers=c(1,2,2,3,7,14,37,22,22),year=c(2015,2016,2017,2018,2019,2020,2021,2022,2022),predicted=c('Published','Published','Published','Published','Published','Published','Published','Published','Predicted'))

ggplot(df, aes(x=as.factor(year),y=n_papers,fill=as.factor(predicted))) + 
  geom_col() +
  theme_bw() +
  scale_fill_manual(values = c("#E6AF2E","#007991")) +
  labs(x = 'Year of publication', y = 'Number of Publications', fill = '') +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 11))

# Specification selection ----

df <- data.frame(n_papers=c(3,37,34,50,31,36,0,4),specification=c('Data collection','Data pre-processing','Predictor variables','Control variables','Outcome variables','Model types','','Sampling'),col=c('b','b','b','b','b','b','b','o'))
df$specification <- factor(df$specification, levels = df$specification)

ggplot(df, aes(x=specification,y=n_papers,fill=col)) + 
  geom_col() +
  scale_fill_manual(values = c("#007991","#E6AF2E")) +
  theme_bw() +
  coord_flip() +
  labs(x = '', y = 'Number of Publications', fill = '') +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none",
        text = element_text(family = 'font', size = 14))

# Multiverse analysis visualizations ----

df <- data.frame(n_papers=rev(c(1,6,10,9,1,1,6,35,5,0,1,2,0,6)),specification=rev(c('Multiverse computation schematic','Outcome matrix','Outcome histogram','* Outcome scatterplot','* Outcome boxplots','Universe specification panel','Outcome curve','Descriptive specification curve','Vibration of effects plot',' ','Explorable multiverse analysis report','Boba','','No visualization')),col=rev(c('b','b','b','b','b','b','b','b','b','b','y','y','y','g')))
df$specification <- factor(df$specification, levels = df$specification)

ggplot(df, aes(x=specification,y=n_papers,fill=col)) + 
  geom_col() +
  scale_fill_manual(values = c("#007991","Grey","#E6AF2E"), name = 'Visualization type', labels = c('Static','None','Dynamic')) +
  theme_bw() +
  coord_flip() +
  labs(x = '', y = 'Number of publications', fill = '') +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 14))

# Deduction and induction methods ----

df <- data.frame(n_papers=rev(c(26,7,37)),specification=rev(c('Specification curve analysis','Vibration of effects','Descriptive statistics only')))
df$specification <- factor(df$specification, levels = df$specification)

ggplot(df, aes(x=specification,y=n_papers)) + 
  geom_col(fill="#007991") +
  theme_bw() +
  coord_flip() +
  labs(x = '', y = 'Number of publications', fill = '') +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 14))
