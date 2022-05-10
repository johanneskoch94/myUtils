test_that("read_items_from_gdx works", {
  gdx <- c("~/R_projects/msg_model/results/gdx_loadpoints/region_model/SSP2-Base_CAZ_msg.gdx")
  read_items_from_gdxs(gdx_filepaths = gdx,
                       gdx_items = list(list(name = "l", field = "l")),
                       remind_names = FALSE)->x
})

read_items_from_gdxs("~/R_projects/msg_model/results/planner_e_ces_model/gdx_files/SSP2-Base_constant_cte_emission_msg.gdx",
                     gdx_items = list(list(name = "l_sol", field = "l"),
                                      list(name = "e_sol_fe", field = "l"),
                                      list(name = "k_agg", field = "l"),
                                      list(name = "x_agg", field = "l"),
                                      list(name = "Yr", field = "l"),
                                      list(name = "y_sol", field = "l"),
                                      list(name = "yl", field = "l"),
                                      list(name = "C_agg", field = "l"),
                                      list(name = "c_sol", field = "l"),
                                      list(name = "U", field = "l"),
                                      list(name = "pe", field = "l"),
                                      list(name = "phi_e", field = "l"),
                                      list(name = "pe", field = "l"),
                                      list(name = "E_agg", field = "l"),
                                      list(name = "emission", field = "l")),
                     remind_names = FALSE)
