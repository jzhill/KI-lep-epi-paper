# Create directory structure (base R only - nothing loaded yet)
dirs <- c(
  "data-processed",
  "data-raw",
  "outputs",
  "context",
  "Quarto",
  "R"
)

lapply(dirs, function(d) {
  dir.create(file.path(getwd(), d), showWarnings = FALSE, recursive = TRUE)
})

# Bootstrap and load renv
if (!file.exists("renv/activate.R")) {
  renv::init()
} else {
  source("renv/activate.R")
}

cat("\nWelcome to", basename(getwd()), "\n")
