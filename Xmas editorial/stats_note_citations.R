# citation_counts.R
# Retrieve citation counts for a vector of DOIs using OpenAlex.
# Requires R >= 4.1 and the httr2 package:  install.packages("httr2")

library(httr2)
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
library(ggtext)

# Strip URL/"doi:" prefixes, whitespace and case so DOIs match across sources
normalize_doi <- function(x) {
  x <- tolower(trimws(x))
  sub("^(https?://(dx\\.)?doi\\.org/|doi:)", "", x)
}

# ---- OpenAlex (batched: up to 50 DOIs per request, free, no API key) -------

get_citation_counts <- function(dois, email = NULL, batch_size = 50) {
  dois    <- unique(normalize_doi(dois))
  batches <- split(dois, ceiling(seq_along(dois) / batch_size))
  
  out <- lapply(batches, function(b) {
    resp <- request("https://api.openalex.org/works") |>
      req_url_query(
        filter     = paste0("doi:", paste(b, collapse = "|")),
        `per-page` = batch_size,
        select     = "doi,cited_by_count",
        mailto     = email
      ) |>
      req_retry(max_tries = 3) |>
      req_perform() |>
      resp_body_json()
    
    if (length(resp$results) == 0) return(NULL)
    do.call(rbind, lapply(resp$results, function(r) {
      data.frame(
        doi = normalize_doi(r$doi),
        citations = r$cited_by_count,
        stringsAsFactors = FALSE
      )
    }))
  })
  
  found <- do.call(rbind, out)
  if (is.null(found)) found <- data.frame(doi = character(), citations = integer())
  
  # Return one row per input DOI, in the original order; NA if not found
  data.frame(
    doi       = dois,
    citations = found$citations[match(dois, found$doi)],
    stringsAsFactors = FALSE
  )
}


# read in citation data
DAT <- read.csv('original_stats_notes_with_url_doi.csv')

counts <- get_citation_counts(DAT$DOI)
print(counts)

DAT <- cbind(DAT, citations=counts$citations)

### Create Figure 2
xmas_green <- "#165B33"
xmas_red   <- "#BB2528"

# ---- Prepare data ----
citations.1000 <- DAT[order(DAT$citations, decreasing = T),]
citations.1000 <- citations.1000[1:sum(citations.1000$citations > 1000), ]

plot_df <- citations.1000 %>%
  mutate(
    title_short = Topic.title %>%
      str_replace("Calculating correlation coefficients with repeated observations: ",
                  "Correlation w/ repeated obs., ") %>%
      str_replace("Analysing controlled trials with baseline and follow-up measurements",
                  "Controlled trials: baseline & follow-up") %>%
      str_wrap(width = 50),
    label = paste0(title_short, "\n", First.author, " & ", Last.author, ", ", Year),
    label = reorder(label, citations),
    top   = citations == max(citations),
      value_label = comma(citations)
  )

# ---- Plot ----
p <- ggplot(plot_df, aes(x = citations, y = label, fill = top)) +
  geom_col(width = 0.7) +
  geom_richtext(aes(label = value_label),
                hjust = -0.1, size = 3.5, colour = xmas_red, fontface = "bold",
                fill = NA, label.colour = NA) +
  scale_fill_manual(values = c("FALSE" = xmas_green, "TRUE" = xmas_red),
                    guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)), labels = comma) +
  labs(x = "Number of citations",
    y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y  = element_blank(),
    panel.grid.minor    = element_blank(),
    panel.grid.major.x  = element_line(colour = "#E8DCC4"),
    axis.text.y         = element_text(colour = "grey15", lineheight = 0.9),
    axis.title.x        = element_text(margin = margin(t = 10)),
    plot.title.position = "plot",
    plot.title          = element_markdown(face = "bold", colour = xmas_green, size = 15),
    plot.subtitle       = element_text(colour = xmas_red),
    plot.margin         = margin(15, 15, 15, 15)
  )

p

summary(DAT$citations)
