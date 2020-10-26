set(CHECK_PROG upperBound)

foreach(prog ${CHECK_PROG})
    add_executable(${prog} ${prog}.c)
    add_test(NAME ${prog} COMMAND ${prog})
endforeach(prog)
