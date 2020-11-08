\section {Performance analysis of the programs}
\label{sec:first}

\subsection {First iteration}
  \label{sec:perf1}

  \begin{lstlisting}
  Samples: 24  of event 'cycles:u', Event count (approx.): 3584609
    Children      Self  Command  Shared Object     Symbol
  +   83.22%     0.00%  game24   game24            [.] _start
  +   83.22%     0.00%  game24   libc-2.31.so      [.] __libc_start_main
  +   83.22%    29.23%  game24   game24            [.] main
  +   78.44%     0.00%  game24   game24            [.] iterateAllSyntaxTrees (inlined)
  +   49.21%     0.00%  game24   game24            [.] checkAndPrintCallback (inlined)
  +   48.43%    48.43%  game24   game24            [.] evalSyntaxTree
  +    9.27%     0.00%  game24   ld-2.31.so        [.] _start
  +    9.14%     0.39%  game24   ld-2.31.so        [.] _dl_start
  +    8.91%     0.00%  game24   game24            [.] swap (inlined)
  \end{lstlisting}

\subsection {Second iteration}
  \label{sec:perf2}
  \begin{lstlisting}
  Samples: 24  of event 'cycles:u', Event count (approx.): 3039439
    Children      Self  Command  Shared Object     Symbol
  +   80.05%     0.00%  game24   game24            [.] _start
  +   80.05%     0.00%  game24   libc-2.31.so      [.] __libc_start_main
  +   80.05%    28.00%  game24   game24            [.] main
  +   76.09%     0.00%  game24   game24            [.] iterateAllSyntaxTrees (inlined)
  +   56.19%    56.19%  game24   game24            [.] evalSyntaxTree
  +   48.09%     0.00%  game24   game24            [.] checkAndPrintCallback (inlined)
  +   11.79%     0.11%  game24   ld-2.31.so        [.] _dl_start
  +   11.74%     0.00%  game24   ld-2.31.so        [.] _start
  +   11.32%     2.04%  game24   ld-2.31.so        [.] _dl_sysdep_start
  +    8.28%     0.00%  game24   ld-2.31.so        [.] dl_main
  +    8.10%     0.00%  game24   [stack]           [.] 0x00007ffec9b0989b
  \end{lstlisting}

\section{Optimization info for the programs}
\label{sec:opt}

\subsection{First iteration}
  \label{sec:opt1}

  \begin{lstlisting}
  ~/src/game24$ gcc -std=c99 -O2 -fopt-info iteration1.c
  iteration1.c:93:20: optimized:  Inlining makeInvalid/12 into evalSyntaxTree/13.
  iteration1.c:92:20: optimized:  Inlining makeNumber/11 into evalSyntaxTree/13.
  iteration1.c:89:14: optimized:  Inlining makeNumber/11 into evalSyntaxTree/13.
  iteration1.c:87:14: optimized:  Inlining makeNumber/11 into evalSyntaxTree/13.
  iteration1.c:85:14: optimized:  Inlining makeNumber/11 into evalSyntaxTree/13.
  iteration1.c:81:14: optimized:  Inlining makeInvalid/12 into evalSyntaxTree/13.
  iteration1.c:76:12: optimized:  Inlining makeNumber/11 into evalSyntaxTree/13.
  iteration1.c:112:5: optimized:   Inlining putchar/2 into printSyntaxTreeImpl/15 (always_inline).
  iteration1.c:108:5: optimized:   Inlining putchar/2 into printSyntaxTreeImpl/15 (always_inline).
  iteration1.c:117:3: optimized:   Inlining putchar/2 into printSyntaxTree/16 (always_inline).
  iteration1.c:160:11: optimized:  Inlining compareOps/19 into iterateAllSyntaxTrees/20.
  iteration1.c:193:7: optimized: converting indirect call in iterateAllSyntaxTrees.constprop/42 to direct call to checkAndPrintCallback/21
  iteration1.c:202:5: optimized:  Inlined printSyntaxTree/16 into checkAndPrintCallback.part.0/41 which now has time 30.000000 and size 13, net change of -6.
  iteration1.c:160:38: optimized:  Inlined incrementOperators/17 into iterateAllSyntaxTrees.constprop/42 which now has time 260776.399170 and size 133, net change of -5.
  iteration1.c:193:7: optimized:  Inlined checkAndPrintCallback/44 into iterateAllSyntaxTrees.constprop/42 which now has time 267554.123535 and size 143, net change of +10.
  optimized:  Inlined checkAndPrintCallback.part.0/45 into checkAndPrintCallback/44 which now has time 269205.316895 and size 17, net change of +5.
  optimized:  Inlined checkAndPrintCallback.part.0/41 into checkAndPrintCallback/21 which now has time 22.232400 and size 22, net change of -8.
  iteration1.c:184:7: optimized:  Inlined swap_impl.constprop/47 into iterateAllSyntaxTrees.constprop/42 which now has time 298341.280762 and size 155, net change of +7.
  iteration1.c:189:7: optimized:  Inlined swap_impl.constprop/48 into iterateAllSyntaxTrees.constprop/42 which now has time 327477.244629 and size 162, net change of +7.
  iteration1.c:186:7: optimized:  Inlined swap_impl.constprop/49 into iterateAllSyntaxTrees.constprop/42 which now has time 356613.208496 and size 169, net change of +7.
  iteration1.c:179:7: optimized:  Inlined swap_impl.constprop/50 into iterateAllSyntaxTrees.constprop/42 which now has time 385749.172363 and size 176, net change of +7.
  iteration1.c:191:7: optimized:  Inlined swap_impl.constprop/51 into iterateAllSyntaxTrees.constprop/42 which now has time 414885.136230 and size 183, net change of +7.
  iteration1.c:181:7: optimized:  Inlined swap_impl.constprop/43 into iterateAllSyntaxTrees.constprop/42 which now has time 444021.100098 and size 190, net change of -8.
  iteration1.c:121:3: optimized: loop with 2 iterations completely unrolled (header execution count 1502253972)
  iteration1.c:170:7: optimized: loop with 2 iterations completely unrolled (header execution count -1856307904)
  iteration1.c:143:3: optimized: loop with 2 iterations completely unrolled (header execution count 1743412647)
  \end{lstlisting}

\subsection{Second iteration}
  \label{sec:opt2}

  \begin{lstlisting}
  ~/src/game24/build$ gcc ../game24.c -O2 -fopt-info -o game24
  ../game24.c:130:5: optimized:   Inlining putchar/5 into printSyntaxTreeImpl/29 (always_inline).
  ../game24.c:126:5: optimized:   Inlining putchar/5 into printSyntaxTreeImpl/29 (always_inline).
  ../game24.c:135:3: optimized:   Inlining putchar/5 into printSyntaxTree/30 (always_inline).
  ../game24.c:111:20: optimized:  Inlining makeInvalid/26 into evalSyntaxTree/27.
  ../game24.c:110:20: optimized:  Inlining makeNumber/25 into evalSyntaxTree/27.
  ../game24.c:107:14: optimized:  Inlining makeNumber/25 into evalSyntaxTree/27.
  ../game24.c:105:14: optimized:  Inlining makeNumber/25 into evalSyntaxTree/27.
  ../game24.c:103:14: optimized:  Inlining makeNumber/25 into evalSyntaxTree/27.
  ../game24.c:99:14: optimized:  Inlining makeInvalid/26 into evalSyntaxTree/27.
  ../game24.c:94:12: optimized:  Inlining makeNumber/25 into evalSyntaxTree/27.
  ../game24.c:386:9: optimized:  Inlining findUChar/44 into hashTree/45.
  ../game24.c:382:9: optimized:  Inlining findUChar/44 into hashTree/45.
  ../game24.c:178:11: optimized:  Inlining compareOps/33 into iterateAllSyntaxTrees/34.
  ../game24.c:230:3: optimized:   Inlining putchar/5 into debugPrintTree/35 (always_inline).
  ../game24.c:217:3: optimized:   Inlining putchar/5 into debugPrintTree/35 (always_inline).
  ../game24.c:211:7: optimized: converting indirect call in iterateAllSyntaxTrees.constprop/84 to direct call to checkAndPrintCallback/48
  ../game24.c:464:20: optimized:  Inlined xmalloc.constprop/85 into main/49 which now has time 111.421311 and size 40, net change of -6.
  ../game24.c:178:38: optimized:  Inlined incrementOperators/31 into iterateAllSyntaxTrees.constprop/84 which now has time 260776.399170 and size 133, net change of -5.
  ../game24.c:446:7: optimized:  Inlined printSyntaxTree/30 into checkAndPrintCallback.part.0/81 which now has time 88.809056 and size 38, net change of -6.
  ../game24.c:438:21: optimized:  Inlined upperBound/46 into checkAndPrintCallback.part.0/81 which now has time 219.098605 and size 46, net change of -8.
  ../game24.c:211:7: optimized:  Inlined checkAndPrintCallback/86 into iterateAllSyntaxTrees.constprop/84 which now has time 267436.181152 and size 142, net change of +9.
  ../game24.c:418:9: optimized:  Inlined xrealloc/24 into insert/47 which now has time 33.453610 and size 35, net change of -7.
  ../game24.c:434:5: optimized:  Inlined canonicalizeTree/43 into checkAndPrintCallback.part.0/81 which now has time 224.104205 and size 56, net change of -8.
  ../game24.c:447:7: optimized:  Inlined insert/47 into checkAndPrintCallback.part.0/81 which now has time 235.001031 and size 80, net change of -11.
  ../game24.c:313:7: optimized:  Inlined maxOperand/87 into rewriteCommutativeChain.constprop/83 which now has time 213.996219 and size 79, net change of +8.
  ../game24.c:313:39: optimized:  Inlined maxOperand/40 into rewriteCommutativeChain.constprop/83 which now has time 207.496219 and size 87, net change of -7.
  optimized:  Inlined analyzeCommutativeOperand.part.0/79 into analyzeCommutativeOperand/37 which now has time 23.206435 and size 37, net change of -9.
  ../game24.c:202:7: optimized:  Inlined swap_impl/88 into iterateAllSyntaxTrees.constprop/84 which now has time 296572.145020 and size 149, net change of +7.
  ../game24.c:207:7: optimized:  Inlined swap_impl/89 into iterateAllSyntaxTrees.constprop/84 which now has time 325708.108887 and size 156, net change of +7.
  ../game24.c:204:7: optimized:  Inlined swap_impl/90 into iterateAllSyntaxTrees.constprop/84 which now has time 354844.072754 and size 163, net change of +7.
  ../game24.c:197:7: optimized:  Inlined swap_impl/91 into iterateAllSyntaxTrees.constprop/84 which now has time 383980.036621 and size 170, net change of +7.
  ../game24.c:209:7: optimized:  Inlined swap_impl/92 into iterateAllSyntaxTrees.constprop/84 which now has time 413116.000488 and size 177, net change of +7.
  ../game24.c:199:7: optimized:  Inlined swap_impl/93 into iterateAllSyntaxTrees.constprop/84 which now has time 442251.964355 and size 184, net change of +7.
  ../game24.c:284:9: optimized:  Inlined swap_impl/94 into sort/39 which now has time 1388.406033 and size 27, net change of +7.
  ../game24.c:350:5: optimized:  Inlined findAdjacentNodes/95 into findCommutativeOperator/42 which now has time 42.640000 and size 56, net change of +13.
  ../game24.c:388:5: optimized:  Inlined swap_impl/96 into hashTree/45 which now has time 559.272765 and size 59, net change of +7.
  ../game24.c:384:5: optimized:  Inlined swap_impl/97 into hashTree/45 which now has time 634.272770 and size 66, net change of +7.
  ../game24.c:250:5: optimized:  Inlined swap_impl/98 into canonicalizeMemoryRepr/36 which now has time 28.150000 and size 30, net change of +7.
  ../game24.c:249:5: optimized:  Inlined swap_impl/99 into canonicalizeMemoryRepr/36 which now has time 36.400000 and size 37, net change of +7.
  ../game24.c:314:5: optimized:  Inlined swap_impl/32 into rewriteCommutativeChain.constprop/83 which now has time 215.746219 and size 94, net change of -8.
  ../game24.c:139:3: optimized: loop with 2 iterations completely unrolled (header execution count 1501659459)
  ../game24.c:188:7: optimized: loop with 2 iterations completely unrolled (header execution count -1088073025)
  ../game24.c:161:3: optimized: loop with 2 iterations completely unrolled (header execution count 1742722696)
  \end{lstlisting}
