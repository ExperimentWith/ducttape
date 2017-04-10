# COMMANDS: the bash commands from some task
submitter shell :: COMMANDS {
  action run {
    bash -c "echo hello"
    #eval "$COMMANDS"
    #bash -c "eval \"$COMMANDS\""
  }
}
