library(httr2)
library(jsonlite)

# API key (set once in your session)
Sys.setenv(OPENAI_API_KEY = "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")
api_key <- Sys.getenv("OPENAI_API_KEY")
stopifnot(nzchar(api_key))

# Test sentence
text <- "Die ersten Lebensjahre sind besonders wichtig für die Entwicklung des Gehirns."

# Fixed categories
categories <- c(
  "Gehirnaufbau",
  "Plastizität",
  "Neuropsychiatrie",
  "Translation",
  "Spezielles/Anderes"
)

# Request body (minimal)
body <- list(
  model = "gpt-4.1-mini",
  input = list(
    list(
      role = "system",
      content = "Ordne den Text GENAU EINER Kategorie zu. Antworte nur mit dem Kategorienamen."
    ),
    list(
      role = "user",
      content = paste(
        "Kategorien:", paste(categories, collapse = ", "),
        "\nText:", text
      )
    )
  ),
  temperature = 0
)

# Call API
resp <- request("https://api.openai.com/v1/responses") |>
  req_headers(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "application/json"
  ) |>
  req_body_json(body) |>
  req_perform()

# Extract result
resp_json <- resp_body_json(resp, simplifyVector = TRUE)
label <- resp_json$output$content[[1]]$text

cat("Satz:", text,"\n", "Kategorie:", label, "\n")
