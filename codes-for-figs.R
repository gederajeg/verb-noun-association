library(tidyverse)

# dir.create("figs")

resp_count <- read_csv("data/data-main.csv")
nouns <- read_csv2("data/noun-codes.csv") |> 
  rename(response_ID = CODES)

resp_count2 <- resp_count |> 
  group_by(eng_v, groups, response_ID) |> 
  summarise(n = sum(n), .groups = "drop") |> 
  arrange(groups) |> 
  rename(freq = n) |> 
  left_join(nouns, by = join_by(response_ID))
resp_count2


df <- resp_count2 |> 
  filter(groups == "A") |> 
  mutate(eng_v = fct_relevel(eng_v, c("Take", "Bring", "Collect", "Gather", "Seize", "Fetch", "Hoard")),
         NOUN = fct_relevel(NOUN, c("log", "wheelbarrow", "hammer", "box"))) 
cols <- df$COLS
names(cols) <- df$NOUN

# TAKE group =====
df |> 
  group_by(eng_v) |> 
  mutate(perc = round(freq/sum(freq) * 100, 1)) |> 
  ggplot(aes(x = eng_v, 
             y = perc, 
             fill = NOUN)) + 
  geom_col(position = "dodge") + 
  scale_fill_manual(values = cols) +
  theme_bw() +
  theme(legend.position = "top") + 
  labs(y = "Frequency (%)",
       x = "TAKE verb group (experiment data)",
       fill = "")
ggsave("figs/01-TAKE-perc.png")

# MAKE group =====
df <- resp_count2 |> 
  filter(groups == "B") |> 
  mutate(eng_v = fct_relevel(eng_v, c("Make", "Create", "Produce", "Design", "Establish", "Construct", "Manufacture")),
         NOUN = fct_relevel(NOUN, c("carpet", "ornament", "gold bar", "paper airplane"))) 
cols <- df$COLS
names(cols) <- df$NOUN

df |> 
  group_by(eng_v) |> 
  mutate(perc = round(freq/sum(freq) * 100, 1)) |> 
  ggplot(aes(x = eng_v, 
             y = perc, 
             fill = NOUN)) + 
  geom_col(position = "dodge") + 
  scale_fill_manual(values = cols) +
  theme_bw() +
  theme(legend.position = "top") + 
  labs(y = "Frequency (%)",
       x = "MAKE verb group (experiment data)",
       fill = "")
ggsave("figs/02-MAKE-perc.png")

# FIND group ====
df <- resp_count2 |> 
  filter(groups == "C") |> 
  mutate(eng_v = fct_relevel(eng_v, c("Find", "Discover", "Search", "Explore", "Investigate", "Locate", "Rummage")),
         NOUN = fct_relevel(NOUN, c("milk", "items", "puzzle piece", "gold ore"))) 
cols <- df$COLS
names(cols) <- df$NOUN

df |> 
  group_by(eng_v) |> 
  mutate(perc = round(freq/sum(freq) * 100, 1)) |> 
  filter(groups == "C") |> 
  ggplot(aes(x = eng_v, 
             y = perc, 
             fill = NOUN)) + 
  geom_col(position = "dodge") + 
  scale_fill_manual(values = cols) +
  theme_bw() +
  theme(legend.position = "top") + 
  labs(y = "Frequency (%)",
       x = "FIND verb group (experiment)",
       fill = "")
ggsave("figs/03-FIND-perc.png")

# GET group ====
df <- resp_count2 |> 
  filter(groups == "D") |> 
  mutate(eng_v = fct_relevel(eng_v, c("Get", "Receive", "Earn", "Gain", "Obtain", "Acquire", "Procure")),
         NOUN = fct_relevel(NOUN, c("money", "medal", "trophy", "television"))) 
cols <- df$COLS
names(cols) <- df$NOUN

df |> 
  group_by(eng_v) |> 
  mutate(perc = round(freq/sum(freq) * 100, 1)) |> 
  ggplot(aes(x = eng_v, 
             y = perc, 
             fill = NOUN)) + 
  geom_col(position = "dodge") + 
  scale_fill_manual(values = cols) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(y = "Frequency (%)",
       x = "GET verb group (experiment data)",
       fill = "")
ggsave("figs/04-GET-perc.png")

# GIVE group ====
df <- resp_count2 |> 
  filter(groups == "E") |> 
  mutate(eng_v = fct_relevel(eng_v, c("Give", "Provide", "Share", "Present", "Grant", "Donate", "Bestow")),
         NOUN = fct_relevel(NOUN, c("food", "t-shirt", "gift", "cup"))) 
cols <- df$COLS
names(cols) <- df$NOUN

df |>  
  group_by(eng_v) |> 
  mutate(perc = round(freq/sum(freq) * 100, 1)) |> 
  filter(groups == "E") |> 
  ggplot(aes(x = eng_v, 
             y = perc, 
             fill = NOUN)) + 
  geom_col(position = "dodge") + 
  scale_fill_manual(values = cols) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(y = "Frequency (%)",
       x = "GIVE verb group (experiment data)",
       fill = "")
ggsave("figs/05-GIVE-perc.png")
