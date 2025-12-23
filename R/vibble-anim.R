


render_animation <- function(p,
                             filename = NULL,
                             folder = getwd(),
                             zstack = waiver(),
                             title = waiver(),
                             subtitle = waiver(),
                             caption = waiver(),
                             verbose = vbl_opts("verbose"),
                             ...){


  # 1. sanity checks

  # file writing
  if(is.character(filename)){

    .check_allow_write()

    if(!fs::is_absolute_path(filename)){

      stopifnot(fs::dir_exists(folder))

      filename <- file.path(folder, filename)

    }

    filetype <- stringr::str_extract(filename, pattern = "\\.gif$|\\.mp4$")

    if(!filetype %in% c(".gif", ".mp4")){

      rlang::abort(
        message = c(
          "Invalid filetype for animation rendering.",
          i = "`filename` must end with '.gif' and '.mp4'."
        )
      )

    }

  }

  # continuous sequence
  .stop_if_not(is_slice_seq(slices(p)))

  # no offset
  if(is_offset(p)){

    .glue_message("Reversing offset for animation.", verbose = verbose)

    p$vbl2D <- reverse_offset(p$vbl2D)

  }

  # 2. final adjustments
  if(is.character(zstack)){

    .glue_message("Forcing zstack: {zstack}", verbose = verbose)

    p$vbl2D <- apply_zstack(p$vbl2D, zstack = zstack)

  }

  .glue_message("Preparing animation. This can take a few seconds.", verbose = verbose)

  g <-
    as_ggplot(p, animate = TRUE) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    gganimate::transition_manual(slice)

  if(filetype == ".gif"){

    renderer <- gganimate::gifski_renderer(filename)

  } else if(filetype == ".mp4"){

    renderer <- gganimate::av_renderer(filename)

  }

  anim <- gganimate::animate(g, renderer = renderer, ...)

  return(anim)

}


