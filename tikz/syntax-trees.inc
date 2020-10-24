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
      child{ node{$\cdot$}
        child{ node{$2$} }
        child{ node{$3$} }
      }
      child{ node{$4$} }
    };
  \end{tikzpicture}
  \caption{Examples of syntax trees}
  \label{fig:trees}
\end{figure}