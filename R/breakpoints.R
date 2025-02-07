isR6ClassGenerator <- function(x) {
  inherits(x, "R6ClassGenerator")
}

setBreakpoints <- function(
  sourceBreakpoints,
  unsetBreakpoints = FALSE,
  envs = list(),
  inNormalEnvs = TRUE
) {
  path <- sourceBreakpoints$source$path
  bps <- sourceBreakpoints$breakpoints
  refList <- list()

  for (i in seq_along(bps)) {
    bp <- bps[[i]]
    line <- bp$requestedLine

    # Try to find the breakpoint location normally in all given environments.
    refs <- list()
    for (env in envs) {
      newRefs <- try(
        findLineNum(path, line, nameonly = FALSE, envir = env, lastenv = env),
        silent = TRUE
      )
      if (!inherits(newRefs, "try-error") && length(newRefs) > 0) {
        refs <- c(refs, newRefs)
      }
    }

    # If no references were found, try to see if any environment holds an R6 generator
    # that defines the method (e.g. inside its public_methods list)
    if (length(refs) == 0) {
      for (env in envs) {
        for (objName in ls(env, all.names = TRUE)) {
          obj <- env[[objName]]
          if (isR6ClassGenerator(obj)) {
            # Instead of direct indexing, check if public_methods exists and bp$name is among its names
            if (!is.null(obj$public_methods) && bp$name %in% names(obj$public_methods)) {
              newEnv <- obj$public_methods
              newRefs <- try(
                findLineNum(path, line, nameonly = FALSE, envir = newEnv, lastenv = newEnv),
                silent = TRUE
              )
              if (!inherits(newRefs, "try-error") && length(newRefs) > 0) {
                refs <- c(refs, newRefs)
                # Update breakpoint’s environment so that later trace() applies in the public_methods env
                bp$env <- newEnv
                break
              }
            }
          }
        }
        if (length(refs) > 0) break
      }
    }

    # Add found references for this breakpoint to our list.
    refList <- c(refList, refs)

    # Update breakpoint info if at least one reference was found.
    if (length(refs) > 0) {
      bp$verified <- !unsetBreakpoints
      bp$line <- refs[[1]]$line
      bp$at <- refs[[1]]$at  # store the location info
      bp$changed <- TRUE
    }
    bp$attempted <- TRUE
    bps[[i]] <- bp
  }

  # Summarize refs (group breakpoints in the same function)
  summarizedRefs <- summarizeRefs(refList)

  # Set (or remove) breakpoints via trace()/untrace() calls.
  for (sRef in summarizedRefs) {
    if (unsetBreakpoints) {
      suppressMessages(try(
        untrace(
          what = sRef$name,
          where = sRef$env
        ),
        silent = TRUE
      ))
    } else {
      suppressMessages(try(
        trace(
          what = sRef$name,
          tracer = quote({ vscDebugger::.vsc.preBreakpoint(); browser() }),
          at = sRef$at,
          where = sRef$env
        ),
        silent = TRUE
      ))
      fixSrcrefOnTracedFunction(
        what = sRef$name,
        at = sRef$at,
        where = sRef$env
      )
    }
  }

  # Send updated breakpoint info back to VS Code.
  bps <- sendBreakpoints(bps)

  sourceBreakpoints$breakpoints <- bps

  return(sourceBreakpoints)
}

sendBreakpoints <- function(bps) {
  for (bp in bps) {
    if(!is.null(bp$changed) && bp$changed){
      sendBreakpointEvent("changed", bp)
    }
    bp$changed <- FALSE
  }
  return(bps)
}

summarizeRefs <- function(refList){
  summarizedRefs <- list()
  for(ref in refList){
    found <- FALSE
    for(j in seq_along(summarizedRefs)){
      sRef <- summarizedRefs[[j]]
      if(identical(ref$name, sRef$name) && identical(ref$env, sRef$env)){
        # update sRef
        found <- TRUE
        # avoid adding the same breakpoint twice: (necessary?)
        if (!any(sapply(sRef$at, identical, ref$at))){
          sRef$at <- c(sRef$at, list(ref$at))
          sRef$line <- c(sRef$line, list(ref$line))
          sRef$timediff <- c(sRef$timediff, list(ref$timediff))
          summarizedRefs[[j]] <- sRef
        }
        break
      }
    }
    if(!found){
      # make new sRef entry
      sRef <- list(
        name = ref$name,
        env = ref$env,
        at = list(ref$at),
        line = list(ref$line),
        timediff = list(ref$timediff)
      )
      summarizedRefs <- c(summarizedRefs, list(sRef))
    }
  }
  return(summarizedRefs)
}

fixSrcref <- function(f, at){
  at0 <- at[ -length(at) ]
  at1 <- at[ length(at) ]
  if(length(at)==0){
    return(f)
  } else if(length(at)==1){
    b <- body(f)
  } else{
    b <- body(f)[[at0]]
  }
  sr <- attr(b, 'srcref')[[at1]]
  srNew <- lapply(body(f)[[at]], function(...) sr)
  attr(body(f)[[at]], 'srcref') <- srNew
  return(f)
}

fixSrcrefOnTracedFunction <- function(what, at, where){
  f <- get(what, envir=where)
  if(!is.list(at)){
    at <- list(at)
  }
  f2 <- f@.Data
  for(atEntry in at){
    f2 <- fixSrcref(f2, atEntry)
  }
  f@.Data <- f2
  assignOverBinding(what, f, where, FALSE)
}

