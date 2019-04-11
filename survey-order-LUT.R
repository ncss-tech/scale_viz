
#
# roughly based on https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/ref/?cid=nrcs142p2_054254#orders
#
m <- data.frame(
  order=c(2,3,4),
  approx_scale=c(12000, 24000, 65000),
  MMU_low_ac=c(1.5, 4, 40),
  MMU_high_ac=c(10, 40, 60),
  approx_line_width_ft=c(40 , 80, 215),
  n_30m_px_low=c(7, 18, 180),
  n_30m_px_high=c(45, 180, 2790),
  n_90m_px_low=c(1, 2, 20),
  n_90m_px_high=c(5, 20, 310),
  single_px_size_m=c(150, 280, 450)
)


knitr::kable(m)

