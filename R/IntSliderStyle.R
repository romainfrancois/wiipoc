#' Style for the IntSlider widget
#'
#' @param description_width description width
#' @param handle_color css color for the handle
#'
#' @inheritParams jupyter.widgets.base::Style
#'
#' @export
IntSliderStyle <- function(
  description_width = "",
  handle_color = NULL,

  ...,
  error_call = current_env()
) {
  jupyter.widget.IntSliderStyle$new(
    description_width = description_width,
    handle_color = handle_color,

    ...,
    error_call = error_call
  )
}

jupyter.widget.IntSliderStyle <- R6Class("jupyter.widget.IntSliderStyle", inherit = jupyter.widget.Style,
  public = list(
    initialize = function(
      # IntSliderStyle
      description_width = "",
      handle_color = NULL,

      ...,
      error_call = caller_env()
    ) {

      private$state_ <- update_list(private$state_,
        description_width = unbox(ensure(description_width, is.string)),
        handle_color      = unbox(ensure(handle_color, null_or(is.string)))
      )

      super$initialize(
        # Widget
        `_model_module` = '@jupyter-widgets/controls',
        `_model_module_version` = "2.0.0",
        `_model_name` = "IntSliderModel",
        `_view_module` = '@jupyter-widgets/base',
        `_view_count` = NULL,
        `_view_module_version` = "2.0.0",
        `_view_name` = "IntSliderView",

        ...,
        error_call = error_call
      )
    }
  ),

  active = list(
    description_width = function(x) if (missing(x)) private$state_[["description_width"]] else self$update(description_width = unbox(x)),
    handle_color      = function(x) if (missing(x)) private$state_[["handle_color"]] else self$update(handle_color = unbox(x))
  )
)
