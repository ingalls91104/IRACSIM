PRO which,progname,PATH=path,SILENT=silent,IS_FUNCTION=is_function
  resolve_routine,progname,IS_FUNCTION=is_function,/NO_RECOMPILE
  dum = ROUTINE_INFO(progname,/SOURCE,FUNCTIONS=is_function)
  path = dum.PATH
  IF ~KEYWORD_SET(SILENT) THEN print,path
RETURN
END
