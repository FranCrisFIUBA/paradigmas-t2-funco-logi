package interpreter

enum InterpreterError(message: String) extends Error(message) {
  case PLACEHOLDER
    extends InterpreterError("")
}
