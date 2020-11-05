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
  ~/src/game24/build$ gcc -fopt-info -O2 ../game24.c -o game24
  ../game24.c:123:11: optimized:  Inlining compareOps/29 into iterateAllSyntaxTrees/30.
  ../game24.c:153:7: optimized:  Inlining swap/28 into iterateAllSyntaxTrees/30.
  ../game24.c:151:7: optimized:  Inlining swap/28 into iterateAllSyntaxTrees/30.
  ../game24.c:148:7: optimized:  Inlining swap/28 into iterateAllSyntaxTrees/30.
  ../game24.c:146:7: optimized:  Inlining swap/28 into iterateAllSyntaxTrees/30.
  ../game24.c:143:7: optimized:  Inlining swap/28 into iterateAllSyntaxTrees/30.
  ../game24.c:141:7: optimized:  Inlining swap/28 into iterateAllSyntaxTrees/30.
  ../game24.c:58:61: optimized:  Inlining makeInvalid/23 into evalSyntaxTree/24.
  ../game24.c:58:29: optimized:  Inlining makeNumber/22 into evalSyntaxTree/24.
  ../game24.c:56:14: optimized:  Inlining makeNumber/22 into evalSyntaxTree/24.
  ../game24.c:54:14: optimized:  Inlining makeNumber/22 into evalSyntaxTree/24.
  ../game24.c:52:14: optimized:  Inlining makeNumber/22 into evalSyntaxTree/24.
  ../game24.c:48:14: optimized:  Inlining makeInvalid/23 into evalSyntaxTree/24.
  ../game24.c:43:12: optimized:  Inlining makeNumber/22 into evalSyntaxTree/24.
  ../game24.c:77:5: optimized:   Inlining putchar/5 into printSyntaxTreeImpl/25 (always_inline).
  ../game24.c:73:5: optimized:   Inlining putchar/5 into printSyntaxTreeImpl/25 (always_inline).
  ../game24.c:82:3: optimized:   Inlining putchar/5 into printSyntaxTree/26 (always_inline).
  ../game24.c:155:7: optimized: converting indirect call in iterateAllSyntaxTrees.constprop/54 to direct call to checkAndPrintCallback/31
  ../game24.c:123:38: optimized:  Inlined incrementOperators/27 into iterateAllSyntaxTrees.constprop/54 which now has time 174325.022705 and size 116, net change of -9.
  ../game24.c:155:7: optimized:  Inlined checkAndPrintCallback/55 into iterateAllSyntaxTrees.constprop/54 which now has time 182150.243408 and size 126, net change of +10.
  ../game24.c:164:5: optimized:  Inlined printSyntaxTree/56 into checkAndPrintCallback/55 which now has time 183683.494385 and size 16, net change of +4.
  ../game24.c:164:5: optimized:  Inlined printSyntaxTree/26 into checkAndPrintCallback/31 which now has time 22.030000 and size 20, net change of -6.
  ../game24.c:132:7: optimized: loop with 2 iterations completely unrolled (header execution count -1856307904)
  ../game24.c:108:3: optimized: loop with 2 iterations completely unrolled (header execution count 1743412647)
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
