$ flnm = f$enviroment("PROCEDURE")     ! get current procedure name
$ dir = 'f$parse(flnm,,,"DEVICE")''f$parse(flnm,,,"DIRECTORY")'
$ mc 'dir'lisp.exe -M 'dir'lispinit.mem
