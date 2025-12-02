# ==== Fungsi umum untuk download plot ====
make_download_plot <- function(plot_reactive, filename_prefix) {
  downloadHandler(
    filename = function() {
      paste0(filename_prefix, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = plot_reactive(), width = 8, height = 5, dpi = 300)
    }
  )
}