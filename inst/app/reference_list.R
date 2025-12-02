# # --- Fungsi utilitas untuk membuat package references ---
# render_package_refs <- function(pkgs) {
#   refs_df <- purrr::map_df(pkgs, function(pkg) {
#     cit <- tryCatch(utils::citation(pkg), error = function(e) NULL)
#     if (is.null(cit)) {
#       return(tibble(pkg = pkg, ref = paste0(pkg, ": citation not available")))
#     }
#     
#     txt <- paste0(format(cit, style = "text"), collapse = " ")
#     
#     # Ubah penanda markdown menjadi HTML italic
#     txt <- gsub("_([^_]+)_", "<em>\\1</em>", txt, perl = TRUE)
#     txt <- gsub("\\*([^*]+)\\*", "<em>\\1</em>", txt, perl = TRUE)
#     
#     # Ubah DOI menjadi link
#     txt <- gsub("(doi:\\s*\\S+)", "<a href='https://doi.org/\\1' target='_blank'>\\1</a>", txt, perl = TRUE)
#     
#     # Bersihkan format
#     txt <- gsub("\\s+,", ",", txt)
#     txt <- gsub(",\\s*$", "", txt)
#     txt <- trimws(txt)
#     
#     tibble(pkg = pkg, ref = txt)
#   })
#   
#   # Sorting berdasarkan nama author pertama
#   refs_df <- refs_df %>%
#     mutate(first_word = sub("\\s.*", "", ref)) %>%
#     arrange(first_word)
#   
#   # Buat HTML ordered list
#   refs_html <- paste0(
#     "<ol style='line-height:1.6;'>",
#     paste0(
#       purrr::map_chr(refs_df$ref, ~paste0("<li style='margin-bottom:6px;'>", ., "</li>")),
#       collapse = ""
#     ),
#     "</ol>"
#   )
#   
#   HTML(refs_html)
# }

# --- Fungsi utilitas untuk membuat package references dan referensi manual ---
render_package_refs <- function(pkgs, manual_refs = NULL) {
  # Process package citations
  refs_df <- purrr::map_df(pkgs, function(pkg) {
    cit <- tryCatch(utils::citation(pkg), error = function(e) NULL)
    if (is.null(cit)) {
      return(tibble(pkg = pkg, ref = paste0(pkg, ": citation not available"), type = "package"))
    }
    
    txt <- paste0(format(cit, style = "text"), collapse = " ")
    
    # Ubah penanda markdown menjadi HTML italic
    txt <- gsub("_([^_]+)_", "<em>\\1</em>", txt, perl = TRUE)
    txt <- gsub("\\*([^*]+)\\*", "<em>\\1</em>", txt, perl = TRUE)
    
    # Ubah DOI menjadi link
    txt <- gsub("(doi:\\s*\\S+)", "<a href='https://doi.org/\\1' target='_blank'>\\1</a>", txt, perl = TRUE)
    
    # Bersihkan format
    txt <- gsub("\\s+,", ",", txt)
    txt <- gsub(",\\s*$", "", txt)
    txt <- trimws(txt)
    
    tibble(pkg = pkg, ref = txt, type = "package")
  })
  
  # Add manual references jika ada
  if (!is.null(manual_refs)) {
    manual_df <- tibble(
      pkg = names(manual_refs) %||% paste0("ref_", seq_along(manual_refs)),
      ref = unlist(manual_refs),
      type = "manual"
    )
    refs_df <- bind_rows(refs_df, manual_df)
  }
  
  # Sorting berdasarkan nama author pertama
  refs_df <- refs_df %>%
    mutate(first_word = sub("\\s.*", "", ref)) %>%
    arrange(first_word)
  
  # Buat HTML ordered list
  refs_html <- paste0(
    "<ol style='line-height:1.6;'>",
    paste0(
      purrr::map_chr(refs_df$ref, ~paste0("<li style='margin-bottom:6px;'>", ., "</li>")),
      collapse = ""
    ),
    "</ol>"
  )
  
  HTML(refs_html)
}