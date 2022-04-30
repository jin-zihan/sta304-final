# figure 1

```{r, echo=FALSE, message=FALSE, fig.height = 3, fig.width = 6, fig.cap = "average SAT admission score of institutions"}
college %>%
  filter(SAT_AVG != "NULL") %>%
  ggplot(aes(x=SAT_AVG)) + geom_histogram(color = "black", fill = "white") + theme_classic() +
  labs(title = "Distribution of Average SAT Scores",
       x = "average SAT score range")
```

# figure 2

```{r, include=FALSE}
try <- college %>%
  select(UGDS_WHITE, UGDS_BLACK, UGDS_HISP, UGDS_ASIAN, UGDS_AIAN, UGDS_NHPI)

try <- data.frame(try)

g1 <- ggplot(melt(try), aes(variable, value)) + geom_boxplot() + theme_classic() +
  labs(title = "Distribution of Enrollment Share",
       x = "race") + 
  scale_x_discrete(labels = c('white','black','Hispanic', 'Asian', 'American Indian', 'Native Hawaiian')) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
```

```{r, include=FALSE}
try2 <- college %>%
  select(C150_4_WHITE, C150_4_BLACK, C150_4_HISP, C150_4_ASIAN, C150_4_AIAN, C150_4_NHPI)

try2 <- data.frame(try2)

g2 <- ggplot(melt(try2), aes(variable, value)) + geom_boxplot() + theme_classic() +
  labs(title = "Distribution of 6-year Graduation Rate ",
       x = "race") + 
  scale_x_discrete(labels = c('white','black','Hispanic', 'Asian', 'American Indian', 'Native Hawaiian')) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
```

```{r, include=FALSE}
try3 <- college %>%
  select(TUITIONFEE_IN, TUITIONFEE_OUT)

try3 <- data.frame(try3)

g3 <- ggplot(melt(try3), aes(variable, value)) + geom_boxplot() + theme_classic() +
  labs(title = "Distribution of Tuition ",
       x = "student type") + 
  scale_x_discrete(labels = c('in-state students','out-of-state students')) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
```

```{r, include=FALSE}
try4 <- college %>%
  select(MD_EARN_WNE_P6, MD_EARN_WNE_P8, MD_EARN_WNE_P10)

try4 <- data.frame(try4)

g4 <- ggplot(melt(try4), aes(variable, value)) + geom_boxplot() + theme_classic() +
  labs(title = "Distribution of Median Earnings ",
       x = "year after enrollments") + 
  scale_x_discrete(labels = c('6 years', '8 years', '10 years')) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
```


```{r,echo=FALSE, warning=FALSE, message=FALSE, fig.height = 6, fig.width = 9, fig.align = "center", fig.cap = "key variables description"}
(g1 | g2)  /
  (g3 | g4)
```

# figure 3

```{r, include=FALSE}
# avg sat and 4-year graduation rate, by race

c150_4 <- college %>% 
  select(SAT_AVG, C150_4_WHITE, C150_4_BLACK, C150_4_HISP, C150_4_ASIAN, C150_4_AIAN, C150_4_NHPI) %>%
  filter(C150_4_WHITE != "NULL") %>%
  filter(C150_4_BLACK != "NULL") %>%
  filter(C150_4_HISP != "NULL") %>%
  filter(C150_4_ASIAN != "NULL") %>%
  filter(C150_4_AIAN != "NULL") %>%
  filter(C150_4_NHPI != "NULL") %>%
  filter(SAT_AVG != "NULL") %>%
  mutate(SAT_RANGE =
           case_when(SAT_AVG > 700 & SAT_AVG <= 800 ~ "700-800",
                     SAT_AVG > 800 & SAT_AVG <= 900 ~ "801-900",
                     SAT_AVG > 1000 & SAT_AVG <= 1100 ~ "1001-1100",
                     SAT_AVG > 1100 & SAT_AVG <= 1200 ~ "1101-1200",
                     SAT_AVG > 1200 & SAT_AVG <= 1300 ~ "1201-1300",
                     SAT_AVG > 1300 & SAT_AVG <= 1400 ~ "1301-1400",
                     SAT_AVG > 1400 & SAT_AVG <= 1500 ~ "1401-1500",
                     SAT_AVG > 1500 & SAT_AVG <= 1600 ~ "1501-1600",
                     TRUE ~ "901-1000"))

c150_4$C150_4_WHITE <- as.numeric(c150_4$C150_4_WHITE)
c150_4$C150_4_BLACK <- as.numeric(c150_4$C150_4_BLACK)
c150_4$C150_4_HISP <- as.numeric(c150_4$C150_4_HISP)
c150_4$C150_4_ASIAN <- as.numeric(c150_4$C150_4_ASIAN)
c150_4$C150_4_AIAN <- as.numeric(c150_4$C150_4_AIAN)
c150_4$C150_4_NHPI <- as.numeric(c150_4$C150_4_NHPI)
c150_4$SAT_AVG <- as.numeric(c150_4$SAT_AVG)

c150_4_mean <- c150_4 %>%
  group_by(SAT_RANGE) %>%
  summarise(mean(C150_4_WHITE),
            mean(C150_4_BLACK),
            mean(C150_4_HISP),
            mean(C150_4_ASIAN),
            mean(C150_4_AIAN),
            mean(C150_4_NHPI))

names(c150_4_mean) <- c('SAT_RANGE', 'WHITE', 'BLACK', 'HISP', 'ASIAN', 'AIAN', 'NHPI')

```


```{r, include=FALSE}
SAT_RANGE <- c(   "700-800", "700-800", "700-800", "700-800", "700-800", "700-800",
                  "801-900", "801-900", "801-900", "801-900", "801-900", "801-900",
                  "901-1000", "901-1000", "901-1000", "901-1000", "901-1000", "901-1000",
                  "1001-1100", "1001-1100", "1001-1100", "1001-1100", "1001-1100", "1001-1100",
                  "1101-1200", "1101-1200", "1101-1200", "1101-1200", "1101-1200", "1101-1200",
                  "1201-1300", "1201-1300", "1201-1300", "1201-1300", "1201-1300", "1201-1300",
                  "1301-1400", "1301-1400", "1301-1400", "1301-1400", "1301-1400", "1301-1400",
                  "1401-1500", "1401-1500", "1401-1500", "1401-1500", "1401-1500", "1401-1500",
                  "1501-1600", "1501-1600", "1501-1600", "1501-1600", "1501-1600", "1501-1600")
race <- c("White", "Black", "Hispanic", "Asian", "American Indian/Alaska Native", "Native Hawaiian/Pacific Islander",
          "White", "Black", "Hispanic", "Asian", "American Indian/Alaska Native", "Native Hawaiian/Pacific Islander",
          "White", "Black", "Hispanic", "Asian", "American Indian/Alaska Native", "Native Hawaiian/Pacific Islander",
          "White", "Black", "Hispanic", "Asian", "American Indian/Alaska Native", "Native Hawaiian/Pacific Islander",
          "White", "Black", "Hispanic", "Asian", "American Indian/Alaska Native", "Native Hawaiian/Pacific Islander",
          "White", "Black", "Hispanic", "Asian", "American Indian/Alaska Native", "Native Hawaiian/Pacific Islander",
          "White", "Black", "Hispanic", "Asian", "American Indian/Alaska Native", "Native Hawaiian/Pacific Islander",
          "White", "Black", "Hispanic", "Asian", "American Indian/Alaska Native", "Native Hawaiian/Pacific Islander",
          "White", "Black", "Hispanic", "Asian", "American Indian/Alaska Native", "Native Hawaiian/Pacific Islander")
GRAD_RATE <- c("0.6000000",	"0.4376000",	"0.4706000",	"0.5000000",	"0.0000000",	"0.0000000",
               "0.2048500",	"0.2061500",	"0.2403000",	"0.3916500",	"0.0379000",	"0.0000000",
               "0.3978781",	"0.3134688",	"0.3563062",	"0.4104000",	"0.4433187",	"0.3467219",
               "0.5304162", "0.3643785", "0.4432346", "0.5597454", "0.3666946", "0.4559762",
               "0.6017477",	"0.4499328",	"0.5208816",	"0.6012109",	"0.4486230",	"0.5039178",
               "0.7446011",	"0.6132347",	"0.6715611",	"0.7649316",	"0.6152526",	"0.6717526",
               "0.8359821",	"0.7436893",	"0.7908179",	"0.8482464",	"0.6461143",	"0.7951071",
               "0.9177750",	"0.8438583",	"0.8840250",	"0.9321500",	"0.7793917",	"0.9467583",
               "0.9549444",	"0.9264667",	"0.9435222",	"0.9672222", "0.7746000",	"0.6851889")

df1 <- data.frame(SAT_RANGE, race, GRAD_RATE)

df1$GRAD_RATE <- as.numeric(df1$GRAD_RATE)

```


```{r, include=FALSE}

P1 <- ggplot(df1, aes(x=SAT_RANGE, y = GRAD_RATE, group = race, colour = race)) +
  geom_line() + 
  theme_classic() +
  scale_x_discrete(limits  = unique(df1[["SAT_RANGE"]])) +
  scale_y_continuous(breaks=c(seq(0,1,0.1)), limits = c(0, 1)) +
  labs(title = "D: Race 6-year Graduation Rate",
       x = "average SAT score range",
       y = "6-year completion rate") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

P1
```


```{r, include=FALSE}
# avg sat and tuition fee，by in and out of state student

tuition <- college %>%
  select(SAT_AVG, TUITIONFEE_IN, TUITIONFEE_OUT) %>%
  filter(SAT_AVG != "NULL") %>%
  filter(TUITIONFEE_IN != "NULL") %>%
  filter(TUITIONFEE_OUT != "NULL") %>%
  mutate(SAT_RANGE =
           case_when(SAT_AVG > 700 & SAT_AVG <= 800 ~ "700-800",
                     SAT_AVG > 800 & SAT_AVG <= 900 ~ "801-900",
                     SAT_AVG > 1000 & SAT_AVG <= 1100 ~ "1001-1100",
                     SAT_AVG > 1100 & SAT_AVG <= 1200 ~ "1101-1200",
                     SAT_AVG > 1200 & SAT_AVG <= 1300 ~ "1201-1300",
                     SAT_AVG > 1300 & SAT_AVG <= 1400 ~ "1301-1400",
                     SAT_AVG > 1400 & SAT_AVG <= 1500 ~ "1401-1500",
                     SAT_AVG > 1500 & SAT_AVG <= 1600 ~ "1501-1600",
                     TRUE ~ "901-1000"))

tuition$TUITIONFEE_IN <- as.numeric(tuition$TUITIONFEE_IN)
tuition$TUITIONFEE_OUT <- as.numeric(tuition$TUITIONFEE_OUT)

tuition_mean <- tuition %>%
  group_by(SAT_RANGE) %>%
  summarise(mean(TUITIONFEE_IN),
            mean(TUITIONFEE_OUT))

tuition_mean
```

```{r, include=FALSE}
SAT_RANGE <- c(   "700-800", "700-800", 
                  "801-900", "801-900", 
                  "901-1000", "901-1000", 
                  "1001-1100", "1001-1100",
                  "1101-1200", "1101-1200", 
                  "1201-1300", "1201-1300", 
                  "1301-1400", "1301-1400", 
                  "1401-1500", "1401-1500", 
                  "1501-1600", "1501-1600")
type <- c("in-state students", "out-of-state students",
          "in-state students", "out-of-state students",
          "in-state students", "out-of-state students",
          "in-state students", "out-of-state students",
          "in-state students", "out-of-state students",
          "in-state students", "out-of-state students",
          "in-state students", "out-of-state students",
          "in-state students", "out-of-state students",
          "in-state students", "out-of-state students")
TUITION <- c("8008.00",	"18480.00",
             "15563.90", "17367.55",
             "16360.89",	"20428.96",
             "20657.80",	"24412.22",
             "23122.41",	"28046.04",
             "29644.14",	"35804.96",
             "35761.38",	"43595.22",
             "49077.51",	"52994.55",
             "55709.63",	"55709.63")

df2 <- data.frame(SAT_RANGE, type, TUITION)

df2$TUITION <- as.numeric(df2$TUITION)


```

```{r, include=FALSE}
P2 <- ggplot(df2, aes(x=SAT_RANGE, y = TUITION, group = type, colour = type)) +
  geom_line() + 
  theme_classic() + 
  scale_x_discrete(limits  = unique(df1[["SAT_RANGE"]])) +
  labs(title = "E: In- and Out-of-State Tuition and Fees",
       x = "average SAT score range",
       y = "tuition and required fees") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

P2
```


```{r, include=FALSE}
# avg sat和undergraduate student proportion，by gender

gender <- college %>%
  select(SAT_AVG, UGDS_MEN, UGDS_WOMEN) %>%
  filter(SAT_AVG != "NULL") %>%
  filter(UGDS_MEN != "NULL") %>%
  filter(UGDS_WOMEN != "NULL") %>%
  mutate(SAT_RANGE =
           case_when(SAT_AVG > 700 & SAT_AVG <= 800 ~ "700-800",
                     SAT_AVG > 800 & SAT_AVG <= 900 ~ "801-900",
                     SAT_AVG > 1000 & SAT_AVG <= 1100 ~ "1001-1100",
                     SAT_AVG > 1100 & SAT_AVG <= 1200 ~ "1101-1200",
                     SAT_AVG > 1200 & SAT_AVG <= 1300 ~ "1201-1300",
                     SAT_AVG > 1300 & SAT_AVG <= 1400 ~ "1301-1400",
                     SAT_AVG > 1400 & SAT_AVG <= 1500 ~ "1401-1500",
                     SAT_AVG > 1500 & SAT_AVG <= 1600 ~ "1501-1600",
                     TRUE ~ "901-1000"))

gender$UGDS_WOMEN <- as.numeric(gender$UGDS_WOMEN)
gender$UGDS_MEN <- as.numeric(gender$UGDS_MEN)

gender_mean <- gender %>%
  group_by(SAT_RANGE) %>%
  summarise(mean(UGDS_WOMEN),
            mean(UGDS_MEN))

gender_mean
```

```{r, include=FALSE}
SAT_RANGE <- c(   "700-800", "700-800", 
                  "801-900", "801-900", 
                  "901-1000", "901-1000", 
                  "1001-1100", "1001-1100",
                  "1101-1200", "1101-1200", 
                  "1201-1300", "1201-1300", 
                  "1301-1400", "1301-1400", 
                  "1401-1500", "1401-1500", 
                  "1501-1600", "1501-1600")
gender <- c("female", "male",
            "female", "male",
            "female", "male",
            "female", "male",
            "female", "male",
            "female", "male",
            "female", "male",
            "female", "male",
            "female", "male")
PROP <- c("0.5717000",	"0.4283000",
          "0.5683000",	"0.4317000"	,
          "0.5818454",	"0.4181546",
          "0.5851934",	"0.4124426",
          "0.5804010",	"0.4195992",
          "0.5415137",	"0.4584863",
          "0.4992320",	"0.5007680",
          "0.5465235",	"0.4534765",
          "0.5011684",	"0.4988316"	)

df5 <- data.frame(SAT_RANGE, gender, PROP)

df5$PROP <- as.numeric(df5$PROP)

```

```{r, include=FALSE}
P3 <- ggplot(df5, aes(x=SAT_RANGE, y = PROP, group = gender, colour = gender)) +
  geom_line() + 
  theme_classic() +
  scale_x_discrete(limits  = unique(df1[["SAT_RANGE"]]))  +
  scale_y_continuous(breaks=c(seq(0.4,0.6,0.02)), limits = c(0.4, 0.6)) +
  labs(title = "A: Gender Enrollment Share",
       x = "average SAT score range",
       y = "undergraduate enrollment share") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

P3
```



```{r, include=FALSE}
# AVG SAT and enrollmemt rate, by races

race <- college %>% 
  select(SAT_AVG, UGDS_WHITE, UGDS_BLACK, UGDS_HISP, UGDS_ASIAN, UGDS_AIAN, UGDS_NHPI) %>%
  filter(UGDS_WHITE != "NULL") %>%
  filter(UGDS_BLACK != "NULL") %>%
  filter(UGDS_HISP != "NULL") %>%
  filter(UGDS_ASIAN != "NULL") %>%
  filter(UGDS_AIAN != "NULL") %>%
  filter(UGDS_NHPI != "NULL") %>%
  filter(SAT_AVG != "NULL") %>%
  mutate(SAT_RANGE =
           case_when(SAT_AVG > 700 & SAT_AVG <= 800 ~ "700-800",
                     SAT_AVG > 800 & SAT_AVG <= 900 ~ "801-900",
                     SAT_AVG > 1000 & SAT_AVG <= 1100 ~ "1001-1100",
                     SAT_AVG > 1100 & SAT_AVG <= 1200 ~ "1101-1200",
                     SAT_AVG > 1200 & SAT_AVG <= 1300 ~ "1201-1300",
                     SAT_AVG > 1300 & SAT_AVG <= 1400 ~ "1301-1400",
                     SAT_AVG > 1400 & SAT_AVG <= 1500 ~ "1401-1500",
                     SAT_AVG > 1500 & SAT_AVG <= 1600 ~ "1501-1600",
                     TRUE ~ "901-1000"))

race$UGDS_WHITE <- as.numeric(race$UGDS_WHITE)
race$UGDS_BLACK <- as.numeric(race$UGDS_BLACK)
race$UGDS_HISP <- as.numeric(race$UGDS_HISP)
race$UGDS_ASIAN <- as.numeric(race$UGDS_ASIAN)
race$UGDS_AIAN <- as.numeric(race$UGDS_AIAN)
race$UGDS_NHPI <- as.numeric(race$UGDS_NHPI)
race$SAT_AVG <- as.numeric(race$SAT_AVG)

race_mean <- race %>%
  group_by(SAT_RANGE) %>%
  summarise(mean(UGDS_WHITE),
            mean(UGDS_BLACK),
            mean(UGDS_HISP),
            mean(UGDS_ASIAN),
            mean(UGDS_AIAN),
            mean(UGDS_NHPI))

names(race_mean) <- c('SAT_RANGE', 'WHITE', 'BLACK', 'HISP', 'ASIAN', 'AIAN', 'NHPI')

race_mean

```


```{r, include=FALSE}
SAT_RANGE <- c(   "700-800", "700-800", "700-800", "700-800", "700-800", "700-800",
                  "801-900", "801-900", "801-900", "801-900", "801-900", "801-900",
                  "901-1000", "901-1000", "901-1000", "901-1000", "901-1000", "901-1000",
                  "1001-1100", "1001-1100", "1001-1100", "1001-1100", "1001-1100", "1001-1100",
                  "1101-1200", "1101-1200", "1101-1200", "1101-1200", "1101-1200", "1101-1200",
                  "1201-1300", "1201-1300", "1201-1300", "1201-1300", "1201-1300", "1201-1300",
                  "1301-1400", "1301-1400", "1301-1400", "1301-1400", "1301-1400", "1301-1400",
                  "1401-1500", "1401-1500", "1401-1500", "1401-1500", "1401-1500", "1401-1500",
                  "1501-1600", "1501-1600", "1501-1600", "1501-1600", "1501-1600", "1501-1600")
race <- c("White", "Black", "Hispanic", "Asian", "American Indian/Alaska Native", "Native Hawaiian/Pacific Islander",
          "White", "Black", "Hispanic", "Asian", "American Indian/Alaska Native", "Native Hawaiian/Pacific Islander",
          "White", "Black", "Hispanic", "Asian", "American Indian/Alaska Native", "Native Hawaiian/Pacific Islander",
          "White", "Black", "Hispanic", "Asian", "American Indian/Alaska Native", "Native Hawaiian/Pacific Islander",
          "White", "Black", "Hispanic", "Asian", "American Indian/Alaska Native", "Native Hawaiian/Pacific Islander",
          "White", "Black", "Hispanic", "Asian", "American Indian/Alaska Native", "Native Hawaiian/Pacific Islander",
          "White", "Black", "Hispanic", "Asian", "American Indian/Alaska Native", "Native Hawaiian/Pacific Islander",
          "White", "Black", "Hispanic", "Asian", "American Indian/Alaska Native", "Native Hawaiian/Pacific Islander",
          "White", "Black", "Hispanic", "Asian", "American Indian/Alaska Native", "Native Hawaiian/Pacific Islander")
ENROLL <- c("0.0169000",	"0.82630000",	"0.0389000",	"0.00760000",	"0.001400000",	"0.0009000000",
            "0.1293000",	"0.63699500",	"0.1065400",	"0.01641500",	"0.035975000",	"0.0019650000",
            "0.3272667",	"0.38333889",	"0.1546824",	"0.01919444",	"0.014119444",	"0.0024833333",
            "0.5677825",	"0.14167423",	"0.1351265",	"0.02743026",	"0.008932388",	"0.0035955083",
            "0.6470220",	"0.09055985",	"0.1116560",	"0.04077391",	"0.005137340",	"0.0021846547",
            "0.6474737",	"0.06209053",	"0.1020289",	"0.06360789",	"0.004378947",	"0.0014773684",
            "0.5658587",	"0.04690533",	"0.1134307",	"0.10255333",	"0.001490667",	"0.0013666667",
            "0.5092980",	"0.04885098",	"0.1026647",	"0.13028627",	"0.002107843",	"0.0007470588",
            "0.3710105",	"0.06628421",	"0.1309158",	"0.21922105",	"0.002421053",	"0.0013789474")

df6 <- data.frame(SAT_RANGE, race, ENROLL)

df6$ENROLL <- as.numeric(df6$ENROLL)

```


```{r, include=FALSE}
P4 <- ggplot(df6, aes(x=SAT_RANGE, y = ENROLL, group = race, colour = race)) +
  geom_line() + 
  theme_classic() +
  scale_x_discrete(limits  = unique(df1[["SAT_RANGE"]]))  +
  scale_y_continuous(breaks=c(seq(0,1,0.1)), limits = c(0, 1)) +
  labs(title = "C: Race Enrollment Share",
       x = "average SAT score range",
       y = "undergraduate enrollment share") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

P4

```



```{r, include=FALSE}
# avg sat and median earning, by year

earning <- college %>%
  select(SAT_AVG, MD_EARN_WNE_P6, MD_EARN_WNE_P8, MD_EARN_WNE_P10) %>%
  filter(SAT_AVG != "NULL") %>%
  filter(MD_EARN_WNE_P6 != "NULL") %>%
  filter(MD_EARN_WNE_P8 != "NULL") %>%
  filter(MD_EARN_WNE_P10 != "NULL") %>%
  mutate(SAT_RANGE =
           case_when(SAT_AVG > 700 & SAT_AVG <= 800 ~ "700-800",
                     SAT_AVG > 800 & SAT_AVG <= 900 ~ "801-900",
                     SAT_AVG > 1000 & SAT_AVG <= 1100 ~ "1001-1100",
                     SAT_AVG > 1100 & SAT_AVG <= 1200 ~ "1101-1200",
                     SAT_AVG > 1200 & SAT_AVG <= 1300 ~ "1201-1300",
                     SAT_AVG > 1300 & SAT_AVG <= 1400 ~ "1301-1400",
                     SAT_AVG > 1400 & SAT_AVG <= 1500 ~ "1401-1500",
                     SAT_AVG > 1500 & SAT_AVG <= 1600 ~ "1501-1600",
                     TRUE ~ "901-1000"))

earning$MD_EARN_WNE_P6 <- as.numeric(earning$MD_EARN_WNE_P6)
earning$MD_EARN_WNE_P8 <- as.numeric(earning$MD_EARN_WNE_P8)
earning$MD_EARN_WNE_P10 <- as.numeric(earning$MD_EARN_WNE_P10)


earning_mean <- earning %>%
  group_by(SAT_RANGE) %>%
  summarise(mean(MD_EARN_WNE_P6),
            mean(MD_EARN_WNE_P8),
            mean(MD_EARN_WNE_P10))

earning_mean
```


```{r, include=FALSE}
SAT_RANGE <- c(   "700-800", "700-800", "700-800",
                  "801-900", "801-900", "801-900", 
                  "901-1000", "901-1000", "901-1000", 
                  "1001-1100", "1001-1100", "1001-1100", 
                  "1101-1200", "1101-1200", "1101-1200", 
                  "1201-1300", "1201-1300", "1201-1300", 
                  "1301-1400", "1301-1400", "1301-1400", 
                  "1401-1500", "1401-1500", "1401-1500", 
                  "1501-1600", "1501-1600", "1501-1600")
year <- c("6 years after enrollment", "8 years after enrollment", "10 years after enrollment",
          "6 years after enrollment", "8 years after enrollment", "10 years after enrollment",
          "6 years after enrollment", "8 years after enrollment", "10 years after enrollment",
          "6 years after enrollment", "8 years after enrollment", "10 years after enrollment",
          "6 years after enrollment", "8 years after enrollment", "10 years after enrollment",
          "6 years after enrollment", "8 years after enrollment", "10 years after enrollment",
          "6 years after enrollment", "8 years after enrollment", "10 years after enrollment",
          "6 years after enrollment", "8 years after enrollment", "10 years after enrollment",
          "6 years after enrollment", "8 years after enrollment", "10 years after enrollment")
EARN <- c("36112.00",	"39759.00",	"43075.00",
          "31512.05",	"35855.47",	"39761.26",
          "34393.34",	"38078.58",	"41164.61",
          "39125.69",	"42663.76",	"45998.10",
          "42924.74",	"47101.52",	"50926.25",
          "48030.63",	"53354.71",	"58543.24",
          "54062.93",	"61233.30",	"68003.00",
          "58170.21",	"66835.15",	"73686.10",
          "81237.11",	"85722.56",	"91385.89"	)

df7 <- data.frame(SAT_RANGE, year, EARN)

df7$EARN <- as.numeric(df7$EARN)

```


```{r, include=FALSE}
P5 <- ggplot(df7, aes(x=SAT_RANGE, y = EARN, group = year, colour = year)) +
  geom_line() + 
  theme_classic() +
  scale_x_discrete(limits  = unique(df1[["SAT_RANGE"]]))  +
  scale_y_continuous(breaks=c(seq(30000,95000,5000)), limits = c(30000,95000)) +
  labs(title = "F: Students Median Earning After 6/8/10 Years Enrollment",
       x = "average SAT score range",
       y = "students median earning") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

P5
```



```{r, include=FALSE}
# PRIVATE OR PUBLIC

type <- college %>%
  select(SAT_AVG, CONTROL) %>%
  filter(SAT_AVG != "NULL") %>%
  filter(CONTROL != "NULL") %>%
  mutate(SAT_RANGE =
           case_when(SAT_AVG > 700 & SAT_AVG <= 800 ~ "700-800",
                     SAT_AVG > 800 & SAT_AVG <= 900 ~ "801-900",
                     SAT_AVG > 1000 & SAT_AVG <= 1100 ~ "1001-1100",
                     SAT_AVG > 1100 & SAT_AVG <= 1200 ~ "1101-1200",
                     SAT_AVG > 1200 & SAT_AVG <= 1300 ~ "1201-1300",
                     SAT_AVG > 1300 & SAT_AVG <= 1400 ~ "1301-1400",
                     SAT_AVG > 1400 & SAT_AVG <= 1500 ~ "1401-1500",
                     SAT_AVG > 1500 & SAT_AVG <= 1600 ~ "1501-1600",
                     TRUE ~ "901-1000")) %>%
  mutate(CONTROL_RANGE =
           case_when(CONTROL == "1" ~ "public",
                     CONTROL == "2" ~ "private",
                     CONTROL == "3" ~ "private"))

type
```

```{r, include=FALSE}
type %>%
  filter(SAT_RANGE == "700-800")

type %>%
  filter(SAT_RANGE == "801-900") %>%
  count(CONTROL_RANGE == "public")

type %>%
  filter(SAT_RANGE == "901-1000") %>%
  count(CONTROL_RANGE == "public")

type %>%
  filter(SAT_RANGE == "1001-1100") %>%
  count(CONTROL_RANGE == "public")

type %>%
  filter(SAT_RANGE == "1101-1200") %>%
  count(CONTROL_RANGE == "public")

type %>%
  filter(SAT_RANGE == "1201-1300") %>%
  count(CONTROL_RANGE == "public")

type %>%
  filter(SAT_RANGE == "1301-1400") %>%
  count(CONTROL_RANGE == "public")

type %>%
  filter(SAT_RANGE == "1401-1500") %>%
  count(CONTROL_RANGE == "public")

type %>%
  filter(SAT_RANGE == "1501-1600") %>%
  count(CONTROL_RANGE == "public")
```

```{r, include=FALSE}
SAT_RANGE <- c(   "700-800", "700-800",
                  "801-900", "801-900", 
                  "901-1000", "901-1000",
                  "1001-1100", "1001-1100",
                  "1101-1200", "1101-1200", 
                  "1201-1300", "1201-1300",
                  "1301-1400", "1301-1400",
                  "1401-1500", "1401-1500",
                  "1501-1600", "1501-1600")
type <- c("public", "private", 
          "public", "private",
          "public", "private",
          "public", "private",
          "public", "private",
          "public", "private",
          "public", "private",
          "public", "private",
          "public", "private")
PROP <- c(1,	0,
          0.2, 0.8,
          0.4, 0.6,
          0.395, 0.605,
          0.417, 0.583,
          0.379, 0.621,
          0.4, 0.6,
          0.137, 0.863,
          0, 1)

df8 <- data.frame(SAT_RANGE, type, PROP)
```

```{r, include=FALSE}
P6 <- ggplot(df8, aes(x=SAT_RANGE, y = PROP, group = type, colour = type)) +
  geom_line() + 
  theme_classic() +
  scale_x_discrete(limits  = unique(df1[["SAT_RANGE"]])) +
  labs(title = "B: Proportion of Public and Private Institutions",
       x = "average SAT score range",
       y = "proportion of institutions") + 
  theme(axis.text.x = element_text(angle = 45, hjust=1))

P6
```

```{r,echo=FALSE, warning=FALSE, fig.height = 14, fig.width = 14, fig.align = "center", fig.cap = "characteristics of institutions with different avg SAT score range"}
(P3 | P6) /
  (P4 | P1) /
  (P2 | P5)
```

# figure 4


```{r, warning=FALSE, echo=FALSE, fig.height = 3, fig.width = 5, fig.cap = "comparison between average SAT score"}
asian <- college %>%
  filter(AANAPII == 1)

college_clean$MD_EARN_WNE_P6 <- as.numeric(college_clean$MD_EARN_WNE_P6)
college_clean$MD_EARN_WNE_P8 <- as.numeric(college_clean$MD_EARN_WNE_P8)
college_clean$MD_EARN_WNE_P10 <- as.numeric(college_clean$MD_EARN_WNE_P10)


college_clean %>% ggplot(aes(y = SAT_AVG, x = AANAPII)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(title = "Average SAT Score of AANAPIS and Non-AANAPIS Institutions",
       x = "institution type",
       y = "average SAT score") + 
  scale_x_discrete(labels = c('AANAPIS institutions','non-AANAPIS institutions'))

```

# figure 5

```{r, include=FALSE}
ppp1 <- college_clean %>% ggplot(aes(y = MD_EARN_WNE_P6, x = AANAPII)) + 
  geom_boxplot() + 
  ylim(20000,125000) +
  theme_classic() +
  labs(title = "A: Median Earnings 6 Years After Enrolled",
       x = "institution type",
       y = "median earnings") + 
  scale_x_discrete(labels = c('AANAPIS institutions','non-AANAPIS institutions'))

ppp2 <- college_clean %>% ggplot(aes(y = MD_EARN_WNE_P8, x = AANAPII)) + 
  geom_boxplot() +
  ylim(20000,125000) +
  theme_classic() +
  labs(title = "B: Median Earnings 8 Years After Enrolled",
       x = "institution type",
       y = "median earnings") + 
  scale_x_discrete(labels = c('AANAPIS institutions','non-AANAPIS institutions'))

ppp3 <- college_clean %>% ggplot(aes(y = MD_EARN_WNE_P10, x = AANAPII)) + 
  geom_boxplot() + 
  ylim(20000,125000) +
  theme_classic() +
  labs(title = "C: Median Earnings 10 Years After Enrolled",
       x = "institution type",
       y = "median earnings") + 
  scale_x_discrete(labels = c('AANAPIS institutions','non-AANAPIS institutions'))
```

```{r,echo=FALSE, warning=FALSE, fig.height = 4, fig.width = 13, fig.align = "center", fig.cap = "Median earnings after 6/8/10 years enrollments"}
(ppp1 | ppp2 | ppp3)
```





