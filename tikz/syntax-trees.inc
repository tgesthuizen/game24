\begin{figure}
  \centering
  \begin{tikzpicture}[level distance=1.5cm,
    level 1/.style={sibling distance=3cm},
    level 2/.style={sibling distance=1.5cm}]
    \node (root-first) {$+$}
    child{ node{$\cdot$}
      child{ node{$1$} }
      child{ node{$2$} }
    }
    child{ node{$-$}
      child{ node{$3$} }
      child{ node{$4$} }
    };
    \node[right of = root-first, node distance = 6cm] {$+$}
    child{ node{$1$} }
    child{ node{$-$}
      child{ node{$*$}
        child{ node{$2$} }
        child{ node{$3$} }
      }
      child{ node{$4$} }
    };
  \end{tikzpicture}
  \caption{Examples of syntax trees}
  \label{fig:trees}
\end{figure}