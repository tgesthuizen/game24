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

\begin{figure}[h]
  \centering
  \begin{tikzpicture} [
    box/.style={draw,%
      outer sep=0,%
      align=center},
    bitc/.style={outer sep = 0,
      minimum width= 1cm,
      align = center}
    ]
    \node[box, minimum width=2cm] (h0) {k1};
    \foreach \i/\size/\name [remember=\i as \lastx (initially 0)] in {1/2cm/k2,2/2cm/k3,3/2cm/l1,4/2cm/r1,5/2cm/l2,6/1cm/r2,7/1cm/l3,8/1cm/r3} 
        \node[box, minimum width=\size, right= 0mm of h\lastx] (h\i) {\name};
    \node[bitc, below = 1cm of h0.west] (b0) {0};
    \draw (b0.north) -- +(0,.1);
    \foreach \i [remember=\i as \lasti (initially 0)] in {2,4,6,...,14} {%
      \node[bitc, right = 1cm of b\lasti] (b\i) {\i};
      \draw (b\i.north) -- +(0,.1) -- +(-2,.1);
    }
    \node[bitc, below = 1cm of h8.east] (b15) {15};
    \draw (b15.north) -- +(0,.1) -- + (-1,.1);
  \end{tikzpicture}
  \begin{description}
    \item[k?] Kind of operator \textit{?}.
    \item[l?] Left hand side operand index for operator \textit{?}.
    \item[r?] Right hand side operand index for operator \textit{?}.
  \end{description}
  \caption{Bit layout of a compressed tree in memory}
  \label{fig:comptree}
\end{figure}
