year <- sub("-.*", "", 2018)
note <- sprintf("R package version %s", "0.0.1")

bibentry(bibtype = "Manual",
         title = "{regimeDetectionMethods}: Calculating Regime Detection Measures",
         author = c(person("Jessica", "Burnett"),
                    person("Nathaniel", "Price")
                    ),
         year = year,
         note = note,
         url = "https://github.com/TrashBirdEcology/regimeDetectionMeasures")
