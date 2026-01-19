```{r}
# --- Load Packages ---
library(sf)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

# --- 1. Use the verified folder path ---
plot_dir <- "C:/Users/Supriya Monajigari/Documents/Plot geopacks"

# List geopackages in that folder
plot_files <- list.files(plot_dir, pattern = "\\.gpkg$", full.names = TRUE)

cat("📁 Found geopackages:\n")
print(basename(plot_files))

if (length(plot_files) == 0) {
  stop("❌ No .gpkg files found — check the folder path or capitalization.")
}

# --- 2. Read and classify geopackages ---
plots_list <- lapply(plot_files, function(f) {
  sf_obj <- tryCatch(st_read(f, quiet = TRUE), error = function(e) NULL)
  if (!inherits(sf_obj, "sf")) return(NULL)
  
  sf_obj$file_name <- basename(f)
  sf_obj$Treatment <- case_when(
    str_detect(sf_obj$file_name, "(?i)C\\.gpkg$") ~ "Continuous",
    str_detect(sf_obj$file_name, "(?i)D\\.gpkg$") ~ "Deferred",
    str_detect(sf_obj$file_name, "(?i)E\\.gpkg$") ~ "Exclosed",
    TRUE ~ "Other"
  )
  sf_obj
})

plots_list <- Filter(Negate(is.null), plots_list)
if (length(plots_list) == 0) stop("❌ None of the geopackages could be read as sf objects.")

plots_sf <- do.call(rbind, plots_list)
plots_sf <- st_make_valid(plots_sf)

# --- 3. Prepare collar dataset ---
collar_subset <- clean_collar_dat %>%
  filter(Message.Type %in% c("poll", "client_zap")) %>%
  mutate(Time = ymd_hms(Time.Mountain, quiet = TRUE))

# --- 4. Convert poll data to sf points ---
poll_sf <- collar_subset %>%
  filter(Message.Type == "poll", !is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# Align CRS if needed
if (st_crs(poll_sf) != st_crs(plots_sf)) {
  poll_sf <- st_transform(poll_sf, st_crs(plots_sf))
}

# --- 5. Spatial join: polls within plots ---
poll_in_plots <- st_join(poll_sf, plots_sf, left = FALSE) %>%
  select(Cow_id, Time, Treatment)

# --- 6. Extract shock (client_zap) events ---
zap_events <- collar_subset %>%
  filter(Message.Type == "client_zap") %>%
  select(Cow_id, Time)

# --- 7. Identify encroachments (shock within ±5 min of poll in plot) ---
encroachments <- zap_events %>%
  left_join(poll_in_plots, by = "Cow_id") %>%
  filter(abs(difftime(Time.x, Time.y, units = "mins")) <= 5) %>%
  distinct(Cow_id, Treatment)

# --- 8. Summaries for histograms ---
shock_summary <- zap_events %>%
  group_by(Cow_id) %>%
  summarise(Shock_Count = n(), .groups = "drop")

encroach_summary <- encroachments %>%
  group_by(Cow_id, Treatment) %>%
  summarise(Encroach_Count = n(), .groups = "drop")

# --- 9. Plots ---
ggplot(shock_summary, aes(x = Shock_Count)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(
    title = "Frequency of Shocks per Animal",
    x = "Number of Shocks",
    y = "Number of Animals"
  ) +
  theme_minimal()

ggplot(encroach_summary, aes(x = Encroach_Count, fill = Treatment)) +
  geom_histogram(binwidth = 1, color = "black", position = "dodge") +
  labs(
    title = "Encroachments into Exclosed or Deferred Plots",
    x = "Number of Encroachments (Polls Within ±5 min of Shock)",
    y = "Number of Animals"
  ) +
  theme_minimal()

```

```{r}
# --- Load Packages ---
library(sf)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

# --- 1. Verified geopack folder path ---
plot_dir <- "C:/Users/Supriya Monajigari/Documents/Plot geopacks"

# Read geopackages
plot_files <- list.files(plot_dir, pattern = "\\.gpkg$", full.names = TRUE)
cat("📁 Found geopackages:\n")
print(basename(plot_files))

if (length(plot_files) == 0) stop("❌ No .gpkg files found.")

plots_list <- lapply(plot_files, function(f) {
  sf_obj <- tryCatch(st_read(f, quiet = TRUE), error = function(e) NULL)
  if (!inherits(sf_obj, "sf")) return(NULL)
  
  sf_obj$file_name <- basename(f)
  sf_obj$Treatment <- case_when(
    str_detect(sf_obj$file_name, "(?i)C\\.gpkg$") ~ "Continuous",
    str_detect(sf_obj$file_name, "(?i)D\\.gpkg$") ~ "Deferred",
    str_detect(sf_obj$file_name, "(?i)E\\.gpkg$") ~ "Exclosed",
    TRUE ~ "Other"
  )
  sf_obj
})

plots_list <- Filter(Negate(is.null), plots_list)
plots_sf <- do.call(rbind, plots_list)
plots_sf <- st_make_valid(plots_sf)

# --- 2. Prepare collar data ---
collar_subset <- clean_collar_dat %>%
  filter(Message.Type %in% c("poll", "client_warning")) %>%  # 👈 now using audio cues
  mutate(Time = ymd_hms(Time.Mountain, quiet = TRUE))

# --- 3. Convert poll data to sf points ---
poll_sf <- collar_subset %>%
  filter(Message.Type == "poll", !is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# Reproject if needed
if (st_crs(poll_sf) != st_crs(plots_sf)) {
  poll_sf <- st_transform(poll_sf, st_crs(plots_sf))
}

# --- 4. Spatial join: polls inside plots ---
poll_in_plots <- st_join(poll_sf, plots_sf, left = FALSE) %>%
  select(Cow_id, Time, Treatment)

# --- 5. Extract audio cue (client_warning) events ---
audio_events <- collar_subset %>%
  filter(Message.Type == "client_warning") %>%
  select(Cow_id, Time)

# --- 6. Match audio cues to nearby poll points (same Cow_id, ±5 min) ---
audio_encroachments <- audio_events %>%
  left_join(poll_in_plots, by = "Cow_id") %>%
  filter(abs(difftime(Time.x, Time.y, units = "mins")) <= 15) %>%
  distinct(Cow_id, Treatment)

# --- 7. Summaries ---
audio_summary <- audio_events %>%
  group_by(Cow_id) %>%
  summarise(Audio_Count = n(), .groups = "drop")

encroach_summary <- audio_encroachments %>%
  group_by(Cow_id, Treatment) %>%
  summarise(Encroach_Count = n(), .groups = "drop")

# --- 8. Plots ---
ggplot(audio_summary, aes(x = Audio_Count)) +
  geom_histogram(binwidth = 1, fill = "darkorange", color = "black") +
  labs(
    title = "Frequency of Audio Cues per Animal",
    x = "Number of Audio Cues",
    y = "Number of Animals"
  ) +
  theme_minimal()

ggplot(encroach_summary, aes(x = Encroach_Count, fill = Treatment)) +
  geom_histogram(binwidth = 1, color = "black", position = "dodge") +
  labs(
    title = "Encroachments into Exclosed or Deferred Plots (Audio Cues)",
    x = "Number of Encroachments (Polls Within ±15 min of Audio Cue)",
    y = "Number of Animals"
  ) +
  theme_minimal()

```

```{r}
# --- Load Packages ---
library(sf)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

# --- 1. Folder path for smaller polygons ---
plot_dir <- "C:/Users/Supriya Monajigari/Documents/plot geopacks minus5"

plot_files <- list.files(plot_dir, pattern = "\\.gpkg$", full.names = TRUE)
cat("📁 Found geopackages in 'plot geopacks minus5':\n")
print(basename(plot_files))

if (length(plot_files) == 0)
  stop("❌ No .gpkg files found in 'plot geopacks minus5'.")

# --- 2. Read and use GrTrt column directly ---
plots_list <- lapply(plot_files, function(f) {
  sf_obj <- tryCatch(st_read(f, quiet = TRUE), error = function(e) NULL)
  if (!inherits(sf_obj, "sf")) return(NULL)
  
  sf_obj$file_name <- basename(f)
  # Use GrTrt field to define treatment type
  if ("GrTrt" %in% names(sf_obj)) {
    sf_obj$Treatment <- case_when(
      str_detect(tolower(sf_obj$GrTrt), "excl") ~ "Exclosed",
      str_detect(tolower(sf_obj$GrTrt), "def")  ~ "Deferred",
      TRUE ~ NA_character_
    )
  } else {
    sf_obj$Treatment <- NA_character_
  }
  sf_obj
})

plots_list <- Filter(Negate(is.null), plots_list)
plots_sf <- do.call(rbind, plots_list) |>
  filter(!is.na(Treatment)) |>
  st_make_valid()

cat("✅ Treatments loaded:\n")
print(table(plots_sf$Treatment))


# --- 3. Prepare collar data (polls + audio cues only, Jul 1–Aug 31) ---
phase_start <- as_datetime("2024-07-01 00:00:00")
phase_end   <- as_datetime("2024-08-31 23:59:59")

collar_subset <- clean_collar_dat %>%
  filter(Message.Type %in% c("poll", "client_warning")) %>%  # audio cues + polls
  mutate(Time = ymd_hms(Time.Mountain, quiet = TRUE)) %>%
  filter(Time >= phase_start & Time <= phase_end)

# --- 4. Convert poll data to sf points ---
poll_sf <- collar_subset %>%
  filter(Message.Type == "poll", !is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# --- 5. CRS check & align ---
if (is.na(st_crs(plots_sf))) {
  # assume UTM 11 N (BC/AB area) then transform to WGS 84
  plots_sf <- st_set_crs(plots_sf, 26911) |> st_transform(4326)
}
if (st_crs(poll_sf) != st_crs(plots_sf)) {
  poll_sf <- st_transform(poll_sf, st_crs(plots_sf))
}

# --- 6. Spatial join: polls inside D/E polygons ---
poll_in_plots <- st_join(poll_sf, plots_sf, left = FALSE) %>%
  select(Cow_id, Time, Treatment)

# --- 7. Audio cue events (client_warning) ---
audio_events <- collar_subset %>%
  filter(Message.Type == "client_warning") %>%
  select(Cow_id, Time)

# --- 8. Encroachments (audio cues within ±5 min of poll in D/E plots) ---
audio_encroachments <- audio_events %>%
  left_join(poll_in_plots, by = "Cow_id") %>%
  filter(abs(difftime(Time.x, Time.y, units = "mins")) <= 5) %>%
  distinct(Cow_id, Treatment)

# --- 9. Summaries ---
audio_summary <- audio_events %>%
  group_by(Cow_id) %>%
  summarise(Audio_Count = n(), .groups = "drop")

encroach_summary <- audio_encroachments %>%
  group_by(Cow_id, Treatment) %>%
  summarise(Encroach_Count = n(), .groups = "drop")

# --- 10. Plot summaries (Deferred vs Exclosed) ---
ggplot(encroach_summary, aes(x = Treatment, y = Encroach_Count, fill = Treatment)) +
  stat_summary(fun = "mean", geom = "bar", color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  scale_fill_manual(values = c("Deferred" = "orange", "Exclosed" = "steelblue")) +
  labs(
    title = "Mean Encroachments into Deferred & Exclosed Plots (Audio Cues, Jul 1–Aug 31)",
    x = "Treatment",
    y = "Mean Encroachments per Animal ± SE",
    fill = "Treatment"
  ) +
  theme_minimal(base_size = 14)

```

```{r}
# ============================================================
#   Encroachments into Exclosed VF Areas Over Time
#   Author: Matthew Francis
#   Phase: July 1 – August 31, 2024
# ============================================================

library(sf)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

# --- 1. Load geopackages (Exclosed only) ---
plot_dir <- "C:/Users/Supriya Monajigari/Documents/plot geopacks minus5"
plot_files <- list.files(plot_dir, pattern = "\\.gpkg$", full.names = TRUE)

plots_list <- lapply(plot_files, function(f) {
  sf_obj <- tryCatch(st_read(f, quiet = TRUE), error = function(e) NULL)
  if (!inherits(sf_obj, "sf")) return(NULL)
  if ("GrTrt" %in% names(sf_obj)) {
    sf_obj$Treatment <- case_when(
      str_detect(tolower(sf_obj$GrTrt), "excl") ~ "Exclosed",
      TRUE ~ NA_character_
    )
  } else sf_obj$Treatment <- NA_character_
  sf_obj
})

plots_list <- Filter(Negate(is.null), plots_list)
plots_sf <- do.call(rbind, plots_list) %>%
  filter(Treatment == "Exclosed") %>%
  st_make_valid()

cat("✅ Exclosed polygons loaded:", nrow(plots_sf), "\n")

# --- 2. Collar data filtered to July–August ---
phase_start <- as_datetime("2024-07-01 00:00:00")
phase_end   <- as_datetime("2024-08-31 23:59:59")

collar_subset <- clean_collar_dat %>%
  filter(Message.Type %in% c("poll", "client_warning")) %>%
  mutate(Time = ymd_hms(Time.Mountain, quiet = TRUE)) %>%
  filter(Time >= phase_start & Time <= phase_end)

# --- 3. Poll points ---
poll_sf <- collar_subset %>%
  filter(Message.Type == "poll", !is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

if (st_crs(poll_sf) != st_crs(plots_sf))
  poll_sf <- st_transform(poll_sf, st_crs(plots_sf))

# --- 4. Polls inside Exclosed polygons ---
poll_in_excl <- st_join(poll_sf, plots_sf, left = FALSE) %>%
  select(Cow_id, Time)

# --- 5. Audio cue events ---
audio_events <- collar_subset %>%
  filter(Message.Type == "client_warning") %>%
  select(Cow_id, Time)

# --- 6. Match audio cues to polls (±15 min) to define encroachments ---
audio_encroachments <- audio_events %>%
  left_join(poll_in_excl, by = "Cow_id") %>%
  filter(abs(difftime(Time.x, Time.y, units = "mins")) <= 15) %>%
  mutate(Encroach_Time = Time.x) %>%
  distinct(Cow_id, Encroach_Time)

cat("✅ Encroachments detected:", nrow(audio_encroachments), "\n")

# --- 7. Aggregate by week (or change to day if desired) ---
encroach_time_summary <- audio_encroachments %>%
  mutate(Week = floor_date(Encroach_Time, "week")) %>%
  group_by(Week) %>%
  summarise(
    Encroachments = n(),
    Animals = n_distinct(Cow_id),
    .groups = "drop"
  )

print(encroach_time_summary)

# --- 8. Plot: Encroachments over time ---
ggplot(encroach_time_summary, aes(x = Week, y = Encroachments)) +
  geom_col(fill = "#56B4E9", color = "black") +
  geom_line(aes(y = Animals * 5), color = "darkorange", size = 1) +
  scale_y_continuous(
    name = "Total Encroachments (bars)",
    sec.axis = sec_axis(~./5, name = "Distinct Animals (line)")
  ) +
  labs(
    title = "Encroachments into Exclosed VF Areas Over Time",
    subtitle = "Weekly totals (July 1 – Aug 31 2024)",
    x = "Week starting",
    caption = "Bars = total encroachments | Orange line = number of animals involved"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank()
  )
# --- 9. Save the figure to file ---
ggsave(
  filename = "Encroachments_Exclosed_OverTime_JulyAug2024.png",   # output name
  path = "C:/Users/Supriya Monajigari/Documents",                 # output directory
  width = 10, height = 6, dpi = 300                               # size and quality
)

```

