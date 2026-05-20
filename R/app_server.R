# ============================================================
#  app_server.R  --  Haupt-Server (golem)
# ============================================================

app_server <- function(input, output, session) {
  mod_single_server("single")
  mod_compare_server("compare")
}
