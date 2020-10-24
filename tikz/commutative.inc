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
  \begin{tikzpicture}[%
    box/.style={draw,%
      outer sep = 0,%
      align=center,%
      minimum width=1.5cm,%
      minimum height=1cm},
    level distance=1.5cm,
    level 1/.style={sibling distance=3cm},
    level 2/.style={sibling distance=1.5cm}
    ]
    \node[box] (b00) {1};
    \node[box, right = 0mm of b00] (b01) {2};
    \node[box, right = 0mm of b01] (b02) {3};
    \node[box, right = 0mm of b02] (b03) {4};
    \node[box, right = 0mm of b03] (b04) {$\cdot {\color{gray} \langle 0, 1 \rangle}$};
    \node[box, right = 0mm of b04] (b05) {$+ {\color{gray} \langle 3, 2 \rangle}$};
    \node[box, right = 0mm of b05] (b06) {$- {\color{gray} \langle 4, 5 \rangle}$};

    \node[below = .5cm of b01] {$-$} child{
      node {$\cdot$}
      child{ node{1} }
      child{ node{2} }
    } child {
      node {$+$}
      child{ node{4} }
      child{ node{3} }
    };

    \node[below = 2cm of b05]{$((4 \cdot 3) - (1 + 2))$};

    \node[shape=coordinate, below = 5cm of b00.west] (xl) {};
    \node[shape=coordinate, below = 5cm of b06.east] (xr) {};
    \draw (xl) -- (xr);
    \node[box, below = 5cm of b00] (b10) {1};
    \node[box, right = 0mm of b10] (b11) {2};
    \node[box, right = 0mm of b11] (b12) {3};
    \node[box, right = 0mm of b12] (b13) {4};
    \node[box, right = 0mm of b13] (b14) {$\cdot {\color{gray} \langle 0, 1 \rangle}$};
    \node[box, right = 0mm of b14] (b15) {$+ {\color{gray} \langle 2, 3 \rangle}$};
    \node[box, right = 0mm of b15] (b16) {$- {\color{gray} \langle 4, 5 \rangle}$};

    \node[below = .5cm of b11] {$-$} child{
      node {$\cdot$}
      child{ node{1} }
      child{ node{2} }
    } child {
      node {$+$}
      child{ node{4} }
      child{ node{3} }
    };

    \node[below = 2cm of b15]{$((3 \cdot 4) - (1 + 2))$};
  \end{tikzpicture}
  \caption{Example of tree canonicalization by means of associativity}
  \label{fig:assoccanon}
\end{figure}

\begin{figure}
  \centering
  \begin{tikzpicture}[%
    box/.style={draw,%
      outer sep = 0,%
      align=center,%
      minimum width=1.5cm,%
      minimum height=1cm},
    level distance=1.5cm,
    level 1/.style={sibling distance=3cm},
    level 2/.style={sibling distance=1.5cm}
    ]
    \node[box] (b00) {1};
    \node[box, right = 0mm of b00] (b01) {2};
    \node[box, right = 0mm of b01] (b02) {3};
    \node[box, right = 0mm of b02] (b03) {4};
    \node[box, right = 0mm of b03] (b04) {$\cdot {\color{gray} \langle 0, 1 \rangle}$};
    \node[box, right = 0mm of b04] (b05) {$+ {\color{gray} \langle 3, 2 \rangle}$};
    \node[box, right = 0mm of b05] (b06) {$+ {\color{gray} \langle 4, 5 \rangle}$};

    \node[below = .5cm of b01] (n00) {$+$} child{
      node {$\cdot$}
      child{ node{1} }
      child{ node{2} }
    } child {
      node (n01) {$+$}
      child{ node{4} }
      child{ node{3} }
    };
    \node[draw, shape=ellipse, color=red, fit=(n00) (n01)] {};

    \node[below = 2cm of b05]{$((4 \cdot 3) + (1 + 2))$};

    \node[shape=coordinate, below = 5cm of b00.west] (xl) {};
    \node[shape=coordinate, below = 5cm of b06.east] (xr) {};
    \draw (xl) -- (xr);
    \node[box, below = 5cm of b00] (b10) {1};
    \node[box, right = 0mm of b10] (b11) {2};
    \node[box, right = 0mm of b11] (b12) {3};
    \node[box, right = 0mm of b12] (b13) {4};
    \node[box, right = 0mm of b13] (b14) {$\cdot {\color{gray} \langle 0, 1 \rangle}$};
    \node[box, right = 0mm of b14] (b15) {$+ {\color{gray} \langle 3, 4 \rangle}$};
    \node[box, right = 0mm of b15] (b16) {$+ {\color{gray} \langle 2, 5 \rangle}$};

    \node[below = .5cm of b11] {$+$} child{
      node{3}
    } child {
      node {$+$}
      child{ node{4} }
      child{node {$\cdot$}
        child{ node{1} }
        child{ node{2} }}
    };

    \node[below = 2cm of b15]{$(3 + (4 + (1 \cdot 2)))$};
  \end{tikzpicture}
  \caption{Example of tree canonicalization by means of commutativity}
  \label{fig:commutcanon}
\end{figure}
