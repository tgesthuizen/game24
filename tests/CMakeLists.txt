set(KNOWN_INPUTS excercise-examples)
foreach(input ${KNOWN_INPUTS})
    add_test(NAME check-${input}
             COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/solutionCount.sh
	             "${CMAKE_CURRENT_SOURCE_DIR}/${input}.in"
		     $<TARGET_FILE:game24>)
endforeach()

set(CHECK_PROG upperBound
	       canonicalizeTree
	       canonicalizeNeverTruncates
	       hashTree
	       insert
	       swap)
foreach(prog ${CHECK_PROG})
    add_executable(${prog} ${prog}.c)
    add_test(NAME ${prog} COMMAND ${prog})
endforeach(prog)
