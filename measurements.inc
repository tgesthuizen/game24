\begin{figure}[h]
  \centering
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
  \caption[Perf recording for iteration one]{Excerpt of the perf
    recording of a run of the first iteration program}
  \label{fig:perfit1}
\end{figure}

\begin{figure}[h]
  \centering
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
  \caption[GCC opt-info, iteration 1]{GCC optimization info for the
    first iteration program}
  \label{fig:opt-info-it1}
\end{figure}
