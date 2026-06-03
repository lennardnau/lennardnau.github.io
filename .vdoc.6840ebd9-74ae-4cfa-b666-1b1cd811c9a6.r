#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: false
#| message: false
#| warning: false
#| results: 'asis'

library(tidyverse)
library(glue)
library(readxl)

df <- read_xlsx("projects.xlsx")
df$id <- 1:nrow(df)

for (cat_name in unique(df$category)) {
  cat("\n#### ", cat_name, "\n\n", sep = "")
  df.cat <- filter(df, category == cat_name)

  for (i in 1:nrow(df.cat)) {
    # Authors + links
    authors <- paste0(
      "<a href=",
      str_split(df.cat$author_links[i], ",", simplify = TRUE),
      ' style="color: black; text-decoration: none;">',
      str_split(df.cat$authors[i], ",", simplify = TRUE) |> str_squish(),
      "</a>"
    )
    if (all(str_detect(authors, "<a href=\\."))) {
      authors <- ""
    } else if (length(authors) == 2) {
      authors <- paste0(" <br><em>With ", paste(authors, collapse = " and "), "</em>")
    } else if (length(authors) > 2) {
      authors <- paste0(" <br><em>With ",
                        paste0(paste(authors[-length(authors)], collapse = ", "),
                               ", and ", last(authors)), "</em>")
    } else {
      authors <- paste0(" <br><em>With ", authors, "</em>")
    }

    journal <- ifelse(df.cat$journal[i] == ".", "",
                      paste0("<br><em>Published in ", df.cat$journal[i], "</em>"))
    year    <- ifelse(df.cat$year[i] == ".", "", paste(",", df.cat$year[i]))
    prereg  <- ifelse(df.cat$prereg[i] == ".", "",
                      paste0("&nbsp;<a href=", df.cat$prereg[i], ">[Preregistration]</a>"))
    pap     <- ifelse(df.cat$pap[i] == ".", "",
                      paste0("&nbsp;<a href=", df.cat$pap[i], ">[PAP]</a>"))
    article <- ifelse(df.cat$article[i] == ".", "",
                      paste0("&nbsp;<a href=", df.cat$article[i], ">[Research article]</a>"))
    blog    <- ifelse(df.cat$blog[i] == ".", "",
                      paste0("&nbsp;<a href=", df.cat$blog[i], ">[Blog article]</a>"))

    entry <- glue(
      '<span>{df.cat$title[i]}{authors}{journal}{year}</span><br>
      <a href="#" data-bs-toggle="collapse" data-bs-target="#collapse{df.cat$id[i]}" aria-expanded="false" aria-controls="collapse{df.cat$id[i]}">[Abstract]</a> {prereg} {pap} {article} {blog}
      <div class="collapse" id="collapse{df.cat$id[i]}"><div class="card card-body">{df.cat$abstract[i]}</div></div>'
    ) |> str_replace_all("\\n\\s*", " ")

    cat("- ", entry, "\n", sep = "")
  }
}

#
#
#
#
#
#
