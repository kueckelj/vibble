

#' @export
dcm2niix_convert <- function(path_out,
                             dir_dicom,
                             quiet = FALSE,
                             rm_json = TRUE){

  cmd <- paste(
    "dcm2niix",
    "-z y",
    "-f", shQuote(basename(stringr::str_remove(path_out, pattern = ".nii.gz$"))),
    "-o", shQuote(dirname(path_out)),
    shQuote(dir_dicom),
    ifelse(quiet, "> /dev/null 2>&1", "")
  )

  system(cmd)

  path_json <- stringr::str_replace(path_out, pattern = ".nii.gz", replacement = ".json")

  if(file.exists(path_json) & rm_json){

    file.remove(path_json)

  }

  file.exists(path_out)

}
