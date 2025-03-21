#' Box
#'
#' @param children children widgets
#' @param box_style box style. empty (default) or one of 'success', 'info', 'warning', 'danger', 'primary'.
#' @param description text description of the button
#' @param disabled TRUE if the Button is disabled
#'
#' @inheritParams jupyter.widgets.base::DOMWidget
#'
#' @export
Box <- function(
    # Box
    children = list(),
    box_style = "",
    description = "Click Me",
    disabled = FALSE,

    # DOMWidget
    layout = Layout(),
    style = NULL,
    tabbable = FALSE,
    tooltip = "",
    `_dom_classes` = character(),

    ...,
    error_call = current_env()
  ) {

  jupyter.widget.Box$new(
    # Box
    children = children,
    box_style = box_style,
    description = description,
    disabled = disabled,

    # DOMWidget
    layout = layout,
    style = style,
    tabbable = tabbable,
    tooltip = tooltip,
    `_dom_classes` = `_dom_classes`,

    ...,
    error_call   = error_call
  )
}

jupyter.widget.Box <- R6Class("jupyter.widget.Box", inherit = jupyter.widget.DOMWidget,
  public = list(
    initialize = function(
      children = list(),
      box_style = "",
      description = "",
      disabled = FALSE,

      # DOM Widget
      layout = Layout(),
      style = NULL,
      tabbable = FALSE,
      tooltip = "",
      `_dom_classes` = character(),

      ...,
      error_call = caller_env()
    ) {

      private$children_ <- children

      # set initial state
      accepted_box_style <- c('success', 'info', 'warning', 'danger', 'primary')
      private$state_ <- update_list(private$state_,
        children     = map_chr(children, \(kid) {
          assert_that(inherits(kid, "jupyter.widget.DOMWidget"), msg = "All children must be DOM widgets")
          glue("IPY_MODEL_{kid$comm$id}")
        }),
        box_style    = unbox(arg_match_or_empty(box_style, values = accepted_box_style, error_call = error_call)),
        description  = unbox(ensure(description, is.string)),
        disabled     = unbox(ensure(disabled, rlang::is_scalar_logical))
      )

      super$initialize(
        # DOMWidget
        layout  = layout,
        style   = style,
        tabbable = tabbable,
        tooltip = tooltip,
        `_dom_classes` = `_dom_classes`,

        # Widget
        `_model_module` = unbox('@jupyter-widgets/controls'),
        `_model_module_version` = unbox("2.0.0"),
        `_model_name` = unbox("BoxModel"),
        `_view_module` = unbox('@jupyter-widgets/controls'),
        `_view_count` = NULL,
        `_view_module_version` = unbox("2.0.0"),
        `_view_name` = unbox("BoxView"),

        ...,
        error_call = error_call
      )
    }
  ),

  active = list(
    box_style   = function(x) if (missing(x)) private$state_[["box_style"]] else self$update(box_style = unbox(x)),
    description = function(x) if (missing(x)) private$state_[["description"]] else self$update(description = unbox(x)),
    disabled    = function(x) if (missing(x)) private$state_[["disabled"]] else self$update(disabled = unbox(x)),
    children    = function(x) if (missing(x)) private$children_ else {
      private$children_ <- x
      self$update(children = map_chr(children, \(kid) glue("IPY_MODEL_{kid$comm$id}")))
    }
  ),

  private = list(
    children_ = list()
  )
)
