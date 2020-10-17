% game24 - Solves a game 24 scenario
% Copyright (C) 2020 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
%
% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <https://www.gnu.org/licenses/>.

\begin{figure}
  \centering
  \begin{tikzpicture}[level distance=1.5cm,
    level 1/.style={sibling distance=3cm},
    level 2/.style={sibling distance=1.5cm},
    arena/.style={shape=rectangle,draw,dotted}]

    \node (first1) {1};
    \node[right of=first1] (second1) {2};
    \node[right of=second1] (third1) {3};
    \node[right of=third1] (fourth1) {4};
    \node[arena, fit = (first1) (second1) (third1) (fourth1)] (arena1) {};
    \node[right of=fourth1] (fop1) {$+$};
    \node[right of=fop1] (sop1) {$\cdot$};
    \node[right of=sop1] (top1) {$\div$};

    \node[below of = first1] (first2) {1};
    \node[right of = first2] (third2) {3};
    \node[right of = third2] (fourth2) {4};
    \node[arena, fit = (first2) (third2) (fourth2)] (arena2) {};
    \node[right of = fourth2] (second2) {2};
    \node[right of = second2] (fop2) {$+$};
    \path (fop2) edge [bend left,->] (second2);
    \node[right of = fop2] (sop2) {$\cdot$};
    \node[right of = sop2] (top2) {$\div$};

    \node[below of = first2] (first3) {1};
    \node[right of = first3] (fourth3) {4};
    \node[right of = fourth3] (fop3) {$+$};
    \node[arena, fit = (first3) (fourth3) (fop3)] (arena3) {};
    \node[right of = fop3] (second3) {2};
    \node[right of = second3] (third3) {3};
    \node[right of = third3] (sop3) {$\cdot$};
    \node[right of = sop3] (top3) {$\div$};
    \path (fop3) edge [bend right,->] (second3);
    \path (fop3) edge [bend right,->] (third3);

    \node[below of = first3] (fop4) {$+$};
    \node[right of = fop4] (fourth4) {4};
    \node[arena, fit = (fop4) (fourth4)] (arena4) {};
    \node[right of = fourth4] (first4) {1};
    \node[right of = first4] (second4) {2};
    \node[right of = second4] (third4) {3};
    \node[right of = third4] (sop4) {$\cdot$};
    \node[right of = sop4] (top4) {$\div$};
    \path (fop4) edge [bend right,->] (second4);
    \path (fop4) edge [bend right,->] (third4);
    \path (sop4) edge [bend left,->] (first4);

    \node[below of = fop4] (sop5) {$\cdot$};
    \node[right of = sop5] (fourth5) {4};
    \node[arena, fit = (sop5) (fourth5)] (arena5) {};
    \node[right of = fourth5] (first5) {1};
    \node[right of = first5] (second5) {2};
    \node[right of = second5] (third5) {3};
    \node[right of = third5] (fop5) {$+$};
    \node[right of = fop5] (top5) {$\div$};
    \path (fop5) edge [bend left,->] (second5);
    \path (fop5) edge [bend left,->] (third5);
    \path (sop5) edge [bend right,->] (first5);
    \path (sop5) edge [bend right,->] (fop5);

    \node[below of = sop5] (sop6) {$\cdot$};
    \node[arena, fit = (sop6)] (arena6) {};
    \node[right of = sop6] (fourth6) {4};
    \node[right of = fourth6] (first6) {1};
    \node[right of = first6] (second6) {2};
    \node[right of = second6] (third6) {3};
    \node[right of = third6] (fop6) {$+$};
    \node[right of = fop6] (top6) {$\div$};
    \path (fop6) edge [bend left,->] (second6);
    \path (fop6) edge [bend left,->] (third6);
    \path (sop6) edge [bend right,->] (first6);
    \path (sop6) edge [bend right,->] (fop6);
    \path (top6) edge [bend left,->] (fourth6);

    \node[below of = sop6] (sop7) {$\cdot$};
    \node[right of = sop7] (fourth7) {4};
    \node[right of = fourth7] (first7) {1};
    \node[right of = first7] (second7) {2};
    \node[right of = second7] (third7) {3};
    \node[right of = third7] (fop7) {$+$};
    \node[right of = fop7] (top7) {$\div$};
    \path (fop7) edge [bend left,->] (second7);
    \path (fop7) edge [bend left,->] (third7);
    \path (sop7) edge [bend right,->] (first7);
    \path (sop7) edge [bend right,->] (fop7);
    \path (top7) edge [bend left,->] (fourth7);
    \path (top7) edge [bend left,->] (fop7);

  \end{tikzpicture}
  \caption{Example of constructing a tree using the arena algorithm}
  \label{fig:treecon}
\end{figure}