data_write_error <- function(message, table_name, data,
                             call = sys.call(-1), ...) {
  message <- paste0("Failed trying to write data:\n",
                    paste(utils::capture.output(print(utils::str(data))),
                          collapse = "\n"),
                    sprintf("\nto table '%s':\n", table_name),
                    message)

  ret <- list(message = message, data = data, call = call)
  class(ret) <- c("dettl_data_write_error", "dettl_error", "error", "condition")
  ret
}


