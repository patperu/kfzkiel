

url_stat_jahrbuecher <- function(year) {
  
  base_url <- "https://www.kiel.de"
  url_path <- "/de/kiel_zukunft/statistik_kieler_zahlen/statistische_jahrbuecher.php"
  url <- paste0(base_url, url_path)
  
  data_url <- read_html(url, encoding = "UTF-8") |> 
    html_nodes(".Base-ul--links") |> 
    html_nodes("a") |> 
    html_attr("href") 
  
  data_url <- paste0(base_url, data_url)
  
  tibble(
    url = str_subset(data_url, paste(year, collapse = "|")),
    jahr = as.numeric(str_extract(url, "\\d{4}"))
  )
  
}

extract_kfz_page <- function(xfile, year) {
  
  x <- pdf_text(pdf = xfile)
  
  z <- purrr::map(x, ~{ readr::read_lines(I(.x)) })
  
  page <- which(map_lgl(z, ~{any(str_detect(.x, "Kraftfahrzeuge in den Stadtteilen \\d{4}|Kraftfahrzeuge nach Art und Nutzung in den Stadtteilen \\d{4}"))}))

  # Save KFZ page
  out_file <- paste0("kfz_kiel_", year, ".pdf")
  qpdf::pdf_subset(xfile, pages = page, output = here::here("data", "jb", out_file))
  
  z <- readr::read_lines(I(x[page]))
  
  idx <- which(str_detect(z, c("Altstadt|Kiel insgesamt")))
  
  z <- z[idx[1]:idx[2]]
  
  z <- str_replace(z, "nicht zuzuordnen 1", "nicht_zuzuordnen1")
  z <- str_replace(z, "Kiel insgesamt", "Kiel_insgesamt")
  z <- str_replace(z, "Neumühlen-Dietrichsdorf", "Neumühlen/Dietrichsdorf")
  z <- str_replace_all(z, "\\.", "")
  
  pkw_names <- c("stadtteil", "kfz_ins", "krad",
                 "pkw_ins", "pkw_privat", "pkw_gewerb",
                 "nutz", "anhaenger")
  
  out <- read_table(I(z), col_names = pkw_names) |> 
    mutate(jahr = year, .before = 1)
  
  print(xfile)
  
  return(out)
}
