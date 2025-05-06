library(ggplot2)
library(tidyverse)
library(MuMIn)

d <- read_csv("data/Grocery_Store_Locations.csv", col_names = TRUE)
brands_list <-list("Wakefern Food Corporation",
                   "Whole Foods Market",
                   "AmazonFresh",
                   "The Fresh Market",
                   "Bashas'",
                   "Raley's",
                   "Smart & Final",
                   "Save-A-Lot",
                   "Ingles Markets",
                   "Sprouts Farmers Market",
                   "WinCo Foods",
                   "Southeastern Grocers",
                   "Giant Eagle",
                   "Hy-Vee",
                   "Wegmans",
                   "Trader Joe's",
                   "Meijer",
                   "Target",
                   "Dollar General",
                   "Aldi",
                   "H-E-B",
                   "Publix",
                   "Stop n Shop",
                   "Sam's Club",
                   "Safeway",
                   "CVS",
                   "Walgreens",
                   "Kroger",
                   "Costco Wholesale",
                   "Walmart Supercenter"
)
clean_d <- d |> filter(PRESENT22 == "Yes") |>
  mutate(BRAND = STORENAME %in% brands_list) |>
  mutate(CLOSED = PRESENT22 != PRESENT25) |>
  mutate(PREDATE = PRESENT90 == "Yes" |
           PRESENT95 == "Yes" |
           PRESENT00 == "Yes" |
           PRESENT05 == "Yes" |
           PRESENT08 == "Yes" |
           PRESENT09 == "Yes" |
           PRESENT10 == "Yes" |
           PRESENT11 == "Yes" |
           PRESENT12 == "Yes" |
           PRESENT13 == "Yes" |
           PRESENT14 == "Yes" |
           PRESENT15 == "Yes" |
           PRESENT16 == "Yes" |
           PRESENT17 == "Yes" |
           PRESENT18 == "Yes" ) |>
  select(STORENAME,ZIPCODE,WARD,CLOSED,BRAND,PREDATE)
clean_d$PREDATE <- as.numeric(clean_d$PREDATE)
clean_d$PREDATE <- as.numeric(clean_d$PREDATE)
clean_d[is.na(clean_d)] <- 0
write.csv(clean_d, "data/Cleaned_Grocery.csv", row.names = FALSE)

##
m <- glm(CLOSED ~ ZIPCODE + WARD + BRAND + PREDATE, data = clean_d, family = "binomial", na.action = na.fail)
mod_list <- dredge(m)

# View the top models
print(mod_list)

# Get the best model (lowest AICc)
best_model <- get.models(mod_list, subset = 1)[[1]]
summary(best_model)

#visualizing
mod_list_df <- as.data.frame(mod_list)
mod_list_df$model_id <- paste0("Model_", seq_len(nrow(mod_list_df)))

# Bar plot ordered by AICc
ggplot(mod_list_df, aes(x = reorder(model_id, AICc), y = AICc)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Model Comparison by AICc",
       x = "Model",
       y = "AICc")
