test_that("read_items_from_gdx works", {
  gdx <- "~/R_projects/msg_calibration/tmp_region_single_SSP2-Base_2130_IND/msg_temp.gdx"
  read_items_from_gdxs(gdx_filepaths = gdx,
                       gdx_items = list(list(name = "l", field = "l")),
                       remind_names = FALSE)
})
