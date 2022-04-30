```{r, include = FALSE}
# import raw data
df <- read_csv("/Users/zihan/Desktop/STA304/inputs/data/Most-Recent-Cohorts-Institution.csv")
df <- df %>%
  select(SAT_AVG,C150_4_WHITE, C150_4_BLACK, C150_4_HISP, C150_4_ASIAN, C150_4_AIAN, C150_4_NHPI, TUITIONFEE_IN, TUITIONFEE_OUT, UGDS_WHITE, UGDS_BLACK, UGDS_HISP,UGDS_ASIAN, UGDS_AIAN, UGDS_NHPI, MD_EARN_WNE_P6, MD_EARN_WNE_P8, MD_EARN_WNE_P10, CONTROL, AANAPII, HBCU, PBI)
```

```{r message=FALSE, warning=FALSE, include=FALSE}
df$SAT_AVG <- as.numeric(df$SAT_AVG)
df$C150_4_WHITE <- as.numeric(df$C150_4_WHITE)
df$C150_4_BLACK <- as.numeric(df$C150_4_BLACK)
df$C150_4_HISP <- as.numeric(df$C150_4_HISP)
df$C150_4_ASIAN <- as.numeric(df$C150_4_ASIAN)
df$C150_4_AIAN <- as.numeric(df$C150_4_AIAN)
df$C150_4_NHPI <- as.numeric(df$C150_4_NHPI)
df$TUITIONFEE_IN <- as.numeric(df$TUITIONFEE_IN)
df$TUITIONFEE_OUT <- as.numeric(df$TUITIONFEE_OUT)
df$UGDS_WHITE <- as.numeric(df$UGDS_WHITE)
df$UGDS_BLACK <- as.numeric(df$UGDS_BLACK)
df$UGDS_HISP <- as.numeric(df$UGDS_HISP)
df$UGDS_ASIAN <- as.numeric(df$UGDS_ASIAN)
df$UGDS_AIAN <- as.numeric(df$UGDS_AIAN)
df$UGDS_NHPI <- as.numeric(df$UGDS_NHPI)
df$MD_EARN_WNE_P6 <- as.numeric(df$MD_EARN_WNE_P6)
df$MD_EARN_WNE_P8 <- as.numeric(df$MD_EARN_WNE_P8)
df$MD_EARN_WNE_P10 <- as.numeric(df$MD_EARN_WNE_P10)
df$AANAPII <- as.numeric(df$AANAPII)
df$HBCU <- as.numeric(df$HBCU)
df$PBI <- as.numeric(df$PBI)
```

```{r, include=FALSE}
# clean dataset
summary(!is.na(df))
```

```{r, include=FALSE}
df <- df%>%
  filter(!is.na(SAT_AVG) & !is.na(C150_4_WHITE) & !is.na(C150_4_BLACK) & !is.na(C150_4_HISP) & !is.na(C150_4_ASIAN) & !is.na(C150_4_AIAN) & !is.na(C150_4_NHPI) & !is.na(TUITIONFEE_IN ) & !is.na(TUITIONFEE_OUT) & !is.na(UGDS_WHITE) & !is.na(UGDS_BLACK) & !is.na(UGDS_HISP) & !is.na(UGDS_ASIAN) & !is.na(UGDS_AIAN) & !is.na(UGDS_NHPI) & !is.na(MD_EARN_WNE_P6) & !is.na(MD_EARN_WNE_P8) & !is.na(MD_EARN_WNE_P10) & !is.na(AANAPII) & !is.na(HBCU) & !is.na(PBI))
```

```{r, include=FALSE}
df <- df %>%
  mutate(CONTROL_TYPE = 
           case_when(
             CONTROL == 1 ~ "public",
             TRUE ~ "private"
           ))
```

```{r, , include=FALSE}
# Step0: Divide data into training and testing

## create training and test set
set.seed(514)

# count the number of observations in the data
n <- nrow(df)

# randomly choose 80% as training
training_indices <- sample(1:n, size=round(0.8*n))

# add a column called "rowid" to our original data
df <- df %>% rowid_to_column()

# create a training set
train <- df %>% filter(rowid %in% training_indices)

# create a testing set
test <- df %>% filter(!(rowid %in% training_indices))
```

```{r, include=FALSE}
# step 1: choose a starting model

# full model
model_full <- lm(SAT_AVG ~ +C150_4_WHITE +C150_4_BLACK +C150_4_HISP +C150_4_ASIAN +TUITIONFEE_IN +TUITIONFEE_OUT +UGDS_WHITE +UGDS_BLACK +UGDS_HISP +UGDS_ASIAN +MD_EARN_WNE_P6 +MD_EARN_WNE_P8 +MD_EARN_WNE_P10 +CONTROL_TYPE, data=df)
```



```{r, include=FALSE}

# step 2: ensure no mullticollinearity is present in the model

vif(model_full)
```

```{r, include=FALSE}
model_full_2 <- lm(SAT_AVG ~ +C150_4_WHITE +C150_4_BLACK +C150_4_HISP +C150_4_ASIAN +TUITIONFEE_IN +TUITIONFEE_OUT +UGDS_WHITE +UGDS_BLACK +UGDS_HISP +UGDS_ASIAN +MD_EARN_WNE_P6   +MD_EARN_WNE_P10 +CONTROL_TYPE, data=df)
vif(model_full_2)
```

```{r, include=FALSE}
model_full_3 <- lm(SAT_AVG ~ +C150_4_WHITE +C150_4_BLACK +C150_4_HISP +C150_4_ASIAN +TUITIONFEE_IN +TUITIONFEE_OUT +UGDS_WHITE +UGDS_BLACK +UGDS_HISP +UGDS_ASIAN +MD_EARN_WNE_P6  +CONTROL_TYPE, data=df)
vif(model_full_3)
```

```{r, include=FALSE}
model_full_4 <- lm(SAT_AVG ~ +C150_4_WHITE +C150_4_BLACK +C150_4_HISP +C150_4_ASIAN  +TUITIONFEE_OUT +UGDS_WHITE +UGDS_BLACK +UGDS_HISP +UGDS_ASIAN +MD_EARN_WNE_P6  +CONTROL_TYPE, data=df)
vif(model_full_4)
```

```{r, include=FALSE}
model_full_5 <- lm(SAT_AVG ~ +C150_4_WHITE +C150_4_BLACK +C150_4_HISP +C150_4_ASIAN +TUITIONFEE_OUT +UGDS_BLACK +UGDS_HISP +UGDS_ASIAN +MD_EARN_WNE_P6  +CONTROL_TYPE, data=df)
vif(model_full_5)
```

```{r, include=FALSE}
model_full_6 <- lm(SAT_AVG ~C150_4_BLACK +C150_4_HISP +C150_4_ASIAN +TUITIONFEE_OUT +UGDS_BLACK +UGDS_HISP +UGDS_ASIAN +MD_EARN_WNE_P6  +CONTROL_TYPE, data=df)
vif(model_full_6)
```

```{r, include=FALSE}
# Step3: check the 2 conditions to make sure that we can use residual plots to diagnose the model

# condition 1: draw a scatterplot between yi and y_hat
Fitted_Value <- fitted(model_full_6)
Average_Rating_Point <- train$avg_vote

plot(Fitted_Value, Average_Rating_Point)
```

```{r, include=FALSE}
# condition 2: draw scatterplots between predictors (can only be done for numerical predictors)
pairs(~C150_4_BLACK +C150_4_HISP +C150_4_ASIAN+TUITIONFEE_OUT +UGDS_BLACK +UGDS_HISP +UGDS_ASIAN +MD_EARN_WNE_P6, data=df)
```

```{r, include=FALSE}
## Step 4: use residual plots to identify potential violations against model assumptions(linearity, normality, constant variance, and un-correlatedness)

# residual plot
## residual vs. fitted
res <- rstandard(model_full_6)
y_hat <- fitted(model_full_6)

plot(y_hat, res)
```

```{r, include=FALSE}
# residual vs. predictors
par(mfrow = c(3, 3))

plot(df$C150_4_BLACK, res)
plot(df$C150_4_HISP, res)
plot(df$C150_4_ASIAN, res)
plot(df$TUITIONFEE_OUT, res)
plot(df$UGDS_BLACK, res)
plot(df$UGDS_HISP, res)
plot(df$UGDS_ASIAN, res)
plot(df$MD_EARN_WNE_P6, res)



```

```{r, include=FALSE}
# residual qq plot
qqnorm(res)
qqline(res)
```

```{r, include=FALSE}

# step 5: explore model transformations to correct assumption violations

df$C150_4_HISP = df$C150_4_HISP + 0.0000001
df$C150_4_BLACK = df$C150_4_BLACK + 0.0000001
df$C150_4_ASIAN = df$C150_4_ASIAN + 0.0000001
df$TUITIONFEE_OUT = df$TUITIONFEE_OUT + 0.0000001
df$UGDS_BLACK = df$UGDS_BLACK + 0.0000001
df$UGDS_HISP = df$UGDS_HISP + 0.0000001
df$UGDS_ASIAN = df$UGDS_ASIAN + 0.0000001
df$MD_EARN_WNE_P6 = df$MD_EARN_WNE_P6 + 0.0000001

summary(powerTransform(cbind(df$SAT_AVG,
                             df$C150_4_HISP,
                             df$C150_4_BLACK,
                             df$C150_4_ASIAN,
                             df$TUITIONFEE_OUT,
                             df$UGDS_BLACK,
                             df$UGDS_HISP,
                             df$UGDS_ASIAN,
                             df$MD_EARN_WNE_P6)))
```

```{r, include=FALSE}
train_trans <- train %>%
  mutate(TUITIONFEE_OUT_trans = TUITIONFEE_OUT^(0.5),
         UGDS_BLACK_trans = log(UGDS_BLACK),
         UGDS_HISP_trans = log(UGDS_HISP),
         UGDS_ASIAN_trans = log(UGDS_ASIAN),
         MD_EARN_WNE_P6_trans = MD_EARN_WNE_P6^(-0.5)
  )
```

```{r, include=FALSE}
model_trans_full <- lm(SAT_AVG ~C150_4_BLACK +C150_4_HISP +C150_4_ASIAN +TUITIONFEE_OUT_trans +UGDS_BLACK_trans +UGDS_HISP_trans +UGDS_ASIAN_trans +MD_EARN_WNE_P6_trans  +CONTROL_TYPE, data=train_trans)
```

```{r, include=FALSE}
# re-assess condition 1 and 2 on the transformed model

# condition 1: draw a scatterplot between yi and y_hat
Fitted_Values <- fitted(model_trans_full)
SAT_AVG <- train_trans$SAT_AVG

plot(Fitted_Values, SAT_AVG)

```

```{r, include=FALSE}
# condition 2: draw scatterplots between predictors (can only be done for numerical predictors)
pairs(~C150_4_BLACK +C150_4_HISP +C150_4_ASIAN +TUITIONFEE_OUT_trans +UGDS_BLACK_trans +UGDS_HISP_trans +UGDS_ASIAN_trans +MD_EARN_WNE_P6_trans, data=train_trans)


```

## use residual plots on the transformed model to see whether it is an improvement over the original model

# residual plot
## residual vs. fitted

par(mfrow=c(1,2))

res <- rstandard(model_trans_full)
y_hat <- fitted(model_trans_full)

plot(y_hat, res)
title(main = "Residuals vs Fitted")


qqnorm(res)
qqline(res)

```

```{r, include=FALSE}
# residual vs. predictors
par(mfrow = c(3, 3))

plot(train_trans$C150_4_BLACK, res)
plot(train_trans$C150_4_HISP, res)
plot(train_trans$C150_4_ASIAN, res)
plot(train_trans$TUITIONFEE_OUT_trans, res)
plot(train_trans$UGDS_BLACK_trans, res)
plot(train_trans$UGDS_HISP_trans, res)
plot(train_trans$UGDS_ASIAN_trans, res)
plot(train_trans$MD_EARN_WNE_P6_trans, res)

```

```{r, include=FALSE}
# residual qq plot
qqnorm(res)
qqline(res)
# normality is greatly improved
```

```{r, include=FALSE}
# Step 6: automated selection
model_auto_reduced <- step(model_trans_full, direction="both")
```

```{r, include=FALSE}
summary(model_auto_reduced)
```
