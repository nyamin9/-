############################################################### 1.3. 대표값 추정

setwd("C:\\Users\\yamingu\\Desktop\\statistics data\\")
state <- read.csv("state.csv")

# Population

# 평균
mean(state[['Population']])

# 절사평균
mean(state[['Population']], trim = 0.1)

# 중간값
median(state[['Population']])


# Murder.Rate

#평균
mean(state[['Murder.Rate']])

# 절사평균
mean(state[['Murder.Rate']], trim = 0.1)

# 중간값
median(state[['Murder.Rate']])

# 가중평균
weighted.mean(state[['Murder.Rate']], w = state[['Population']])

# 가중 중간값
# install.packages("matrixStats")
library(matrixStats)
weightedMedian(state[['Murder.Rate']], w = state[['Population']])


################################################################# 1.4. 변이 추정

state <- read.csv("state.csv")

# Population  

# 표준편차
sd(state[['Population']])

# 사분위수범위(IQR)
IQR(state[['Population']])

# 중위절대편차(MAD)
mad(state[['Population']])


########################################################## 1.5. 데이터 분포 탐색

# 1.5.1. 백분위수와 boxplot

#백분위수
quantile(state[['Murder.Rate']], p = c(.05, .25, .5, .75, .95))

#boxplot
boxplot(state[['Population']]/1000000, ylab = 'Population (millions)')


# 1.5.2. 도수분포표와 히스토그램

# 도수분포표
breaks <- seq(from = min(state[['Population']]),
              to = max(state[['Population']]), length = 11)
pop_freq <- cut(state[['Population']], breaks = breaks,
                right = TRUE, include.lowest = TRUE)
table(pop_freq)

# 히스토그램
hist(state[['Population']], breaks = breaks, xlab = 'Population (millions)')


# 1.5.3. 밀도 그림과 추정
# hist 함수에서 freq이 TRUE면 y축이 frequency, FALSE면 밀도

hist(state[['Murder.Rate']], freq = FALSE, xlab = 'Murder Rate (per 100,000)')
lines(density(state[['Murder.Rate']]), lwd = 3, col = 'blue')


############################################ 1.6. 이진 데이터와 범주 데이터 탐색

# 범주형 데이터에서는 간단한 비율이나 퍼센트를 이용해 데이터 설명
dfw = read.csv("dfw_airline.csv")

# 막대그래프
barplot(as.matrix(dfw)/6, cex.axis = 0.8, cex.names = 0.7,
        xlab = 'Cause of Delay', ylab = 'Count')


################################################################## 1.7. 상관관계

# 상관관계 분석
sp500_px <- read.csv('sp500_data.csv')
sp500_sym <- read.csv('sp500_sectors.csv')

etfs <- sp500_px[row.names(sp500_px) > "2012-07-01",
                 sp500_sym[sp500_sym$sector=="etf", 'symbol']]

# install.packages("corrplot")
library(corrplot)
corrplot(cor(etfs), method = 'ellipse')


# 산점도
plot(sp500_px$T, sp500_px$VZ, xlab = 'ATT (T)', ylab = 'Version (VZ)')


##################################################### 1.8. 두개 이상의 변수 탐색

# 1.8.1. 육각형 구간과 등고선 - 수치형 vs 수치형

kc_tax <- read.csv('kc_tax.csv')
library(ggplot2)


# 데이터에서 필요없는 부분 제거
kc_tax0 <- subset(kc_tax, TaxAssessedValue < 750000 &
                    SqFtTotLiving > 100 &
                    SqFtTotLiving < 3500)
nrow(kc_tax0)


# hex_bin
ggplot(kc_tax0, (aes(x=SqFtTotLiving, y=TaxAssessedValue))) + 
  stat_binhex(color = 'white') + 
  theme_bw() + 
  scale_fill_gradient(low = 'white', high = 'black') +
  labs(x = 'Finished Square Feet', y = 'Tax-Assessed Value')


# 등고선
ggplot(kc_tax0, aes(SqFtTotLiving, TaxAssessedValue)) + 
  theme_bw() + 
  geom_point(alpha = 0.1) + 
  geom_density2d(color = 'white') + 
  labs(x = 'Finished Square Feet', y = 'Tax-Assessed Value')


# 분할표
library(descr)

x_tab <- CrossTable(lc_loans$grade, lc_loans$status,
                    prop.c = FALSE, prop.chisq = FALSE, prop.t = FALSE)
x_tab


# Box plot
airline_stats <- read.csv('airline_stats.csv')
boxplot(pct_carrier_delay ~ airline, data = airline_stats, ylim = c(0,50))


# Violin plot
ggplot(data = airline_stats, aes(airline, pct_carrier_delay)) +
  ylim(0,50) +
  geom_violin() + 
  labs(x = '', y = 'Daily % of Delayed Flights')


# 1.8.4. 다변수 시각화
ggplot(subset(kc_tax0, ZipCode %in% c(98188, 98105, 98108, 98126)),
       aes(x=SqFtTotLiving, y = TaxAssessedValue)) +
  stat_binhex(color = 'white') +
  theme_bw() +
  scale_fill_gradient(low = 'white', high = 'blue') +
  labs(x = 'Finished Square Feet', y = 'Tax-Assessed Value') +
  facet_wrap('ZipCode') # 조건화변수지정
