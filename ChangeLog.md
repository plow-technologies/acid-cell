Change Log Acid-Cell
==================


#History Of Changes
=================

*For complete history please view commit log*

*Change History*
+ 0.1.16
  Added better exception throwing on insert case... already exists now throws when a state is already present in the live map.
  
+ 0.1.15
  Figured out hte main problem with the archiving was the need to close the state every once and a while for garbage collection
+ 0.1.14
  updated acid cell to return appropriate stuff for archiveAndHandle
+ 0.1.13
  updated traverse with key to stop using a built in IO fork
  this allows better control over how and when to forkIO in a cell	
  added asynchronous initialization
+ 0.1.12
  removed local branch dep on safecopy	
  changed insertState to use incoming st
+ 0.1.11
  added specificity to err messages
+ 0.1.10
  added exception handling in a lot of places including all traversals
  added strictness annotations to all recordtypes
+ 0.1.9
  File Paths resolved
  be very careful about changing, the reason there is nothing in archiveAndHandle for filepaths is on purpose.
+ 0.1.7

  Changed to use full path names
  Alarm State no longer admits empty keys
+ 0.1.6
  Changed stdormant to use tvar, hopefully will solve the duplicate lock problem
  Added traverseState to generated functions... Allows for asynchronous error ignoring actions on threads.
  
+ 0.1.5
  Fixed path problems and locking problems
+ 0.1.4
  Added createcheckpoint and close
  Added Archive and Handle
+ 0.1.2
  Added Insert and Delete instances...
+ 0.1.1
  Forgot StateMakers
+ 0.1.0
  First release, general functionality present, still no tests


