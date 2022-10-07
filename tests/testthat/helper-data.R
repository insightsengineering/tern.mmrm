# Produces different versions of mmrm_test_data.
get_version <- function(version = c("A", "B")) {
  version <- match.arg(version)
  set.seed(123, kind = "Mersenne-Twister") # Because of `sample` below.
  # nolint start
  mmrm_test_data %>%
    droplevels() %>%
    {
      if (version == "B") {
        dplyr::mutate(
          .,
          # Introduce extra missing response variable values.
          FEV1 = ifelse(
            sample(c(TRUE, FALSE), size = length(FEV1), replace = TRUE, prob = c(0.1, 0.9)),
            NA,
            FEV1
          ),
          # And also covariate values.
          FEV1_BL = ifelse(
            sample(c(TRUE, FALSE), size = length(FEV1_BL), replace = TRUE, prob = c(0.1, 0.9)),
            NA,
            FEV1_BL
          )
        )
      } else {
        # No further changes in version A.
        .
      }
    }
  # nolint end
}
