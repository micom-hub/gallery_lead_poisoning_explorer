# R/buildsyntheticdata.R
#
# Generate random demonstration records without using real patient data.

build_synthetic_data <- function(n_children = 300) {
  if (
    length(n_children) != 1L ||
      !is.numeric(n_children) ||
      !is.finite(n_children) ||
      n_children < 1 ||
      n_children != floor(n_children)
  ) {
    stop("n_children must be a positive whole number.")
  }

  # get all the counties (pulled from SF to clipboard)
  counties <- c(
    'Alcona County',
    'Alger County',
    'Allegan County',
    'Alpena County',
    'Antrim County',
    'Arenac County',
    'Baraga County',
    'Barry County',
    'Bay County',
    'Benzie County',
    'Berrien County',
    'Branch County',
    'Calhoun County',
    'Cass County',
    'Charlevoix County',
    'Cheboygan County',
    'Chippewa County',
    'Clare County',
    'Clinton County',
    'Crawford County',
    'Delta County',
    'Dickinson County',
    'Eaton County',
    'Emmet County',
    'Genesee County',
    'Gladwin County',
    'Gogebic County',
    'Grand Traverse County',
    'Gratiot County',
    'Hillsdale County',
    'Houghton County',
    'Huron County',
    'Ingham County',
    'Ionia County',
    'Iosco County',
    'Iron County',
    'Isabella County',
    'Jackson County',
    'Kalamazoo County',
    'Kalkaska County',
    'Kent County',
    'Keweenaw County',
    'Lake County',
    'Lapeer County',
    'Leelanau County',
    'Lenawee County',
    'Livingston County',
    'Luce County',
    'Mackinac County',
    'Macomb County',
    'Manistee County',
    'Marquette County',
    'Mason County',
    'Mecosta County',
    'Menominee County',
    'Midland County',
    'Missaukee County',
    'Monroe County',
    'Montcalm County',
    'Montmorency County',
    'Muskegon County',
    'Newaygo County',
    'Oakland County',
    'Oceana County',
    'Ogemaw County',
    'Ontonagon County',
    'Osceola County',
    'Oscoda County',
    'Otsego County',
    'Ottawa County',
    'Presque Isle County',
    'Roscommon County',
    'Saginaw County',
    'St. Clair County',
    'St. Joseph County',
    'Sanilac County',
    'Schoolcraft County',
    'Shiawassee County',
    'Tuscola County',
    'Van Buren County',
    'Washtenaw County',
    'Wayne County',
    'Wexford County'
  )

  # Assign random demographic + test data to each synthetic record
  records <- lapply(seq_len(n_children), function(i) {
    n_tests <- sample(1:4, size = 1)

    first_date <- as.Date("2022-01-01") +
      sample.int(1096, size = 1) -
      1L

    date_of_birth <- first_date -
      sample(180:6000, size = 1)

    test_dates <- first_date +
      c(
        0L,
        cumsum(sample(
          35:400,
          size = n_tests - 1L,
          replace = TRUE
        ))
      )

    # Random initial and final states provide varied trajectories.
    initial_result <- if (sample(c(TRUE, FALSE), 1)) {
      runif(1, 3.5, 20)
    } else {
      runif(1, 0.3, 3.4)
    }

    final_result <- if (sample(c(TRUE, FALSE), 1)) {
      runif(1, 3.5, 20)
    } else {
      runif(1, 0.3, 3.4)
    }

    results <- seq(
      initial_result,
      final_result,
      length.out = n_tests
    )

    # Add make internal tests vary
    if (n_tests > 2L) {
      middle <- 2:(n_tests - 1L)

      results[middle] <- results[middle] +
        rnorm(length(middle), sd = 1)
    }

    results <- round(pmax(0.2, results), 1)

    child <- data.frame(
      Name = sprintf("Synthetic Child %05d", i),
      COUNTY = sample(counties, size = 1),
      SEX = sample(c("F", "M"), size = 1),
      DOB = as.character(date_of_birth),
      SPEC_DT = as.character(test_dates),
      SAM_TYP = sample(
        c("C", "V"),
        size = n_tests,
        replace = TRUE
      ),
      PB_RESULT = ifelse(
        results < 1,
        "<1.0",
        sprintf("%.1f", results)
      ),
      stringsAsFactors = FALSE
    )

    # Add nearby venous tests
    if (runif(1) < 0.25) {
      repeat_test <- child[1, , drop = FALSE]

      repeat_test$SPEC_DT <- as.character(
        first_date + sample(1:14, size = 1)
      )

      repeat_test$SAM_TYP <- "V"

      child <- rbind(child, repeat_test)
    }

    child
  })

  data <- do.call(rbind, records)

  data <- data[
    order(data$Name, data$SPEC_DT),
    ,
    drop = FALSE
  ]

  return(data)
}
