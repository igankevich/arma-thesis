\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{gost}[2015/10/04 GOST R 7.0.11-2011]

\LoadClassWithOptions{scrartcl}

% page margins
\RequirePackage[%
    left=25mm,%
    top=20mm,%
    right=10mm,%
    bottom=20mm,%
    a4paper%
]{geometry}

\KOMAoptions{%
    toc=sectionentrydotfill,%
    bibliography=totoc
}

% no bold font in TOC
%\addtokomafont{sectionentry}{\mdseries}

% line and paragraph spacing
\RequirePackage{setspace}
\setstretch{1.5}
\setlength{\parskip}{0pt}
\setlength{\parindent}{2.5em}

% page numbering
\RequirePackage{fancyhdr}
\fancyhf{} % clear all header and footers
\renewcommand{\headrulewidth}{0pt} % remove the header rule
\fancypagestyle{plain}{\fancyhead[C]{\scriptsize\thepage}}
\pagestyle{plain}

% section format and spacing
% N.B. We use 2 baseline skip here because we have one and half spacing
\RedeclareSectionCommand[%
    font=\LARGE\singlespacing\centering,%
    beforeskip=2\baselineskip,%
    afterskip=2\baselineskip%
]{section}

\RedeclareSectionCommand[%
    font=\Large\singlespacing\centering,%
    beforeskip=2\baselineskip,%
    afterskip=2\baselineskip%
]{subsection}

\RedeclareSectionCommand[%
    font=\large\singlespacing\centering,%
    beforeskip=2\baselineskip,%
    afterskip=2\baselineskip%
]{subsubsection}

\RedeclareSectionCommand[%
    font=\normalsize\bfseries,%
    beforeskip=0pt,%
    indent=2.5em
]{paragraph}

\addtokomafont{section}{\clearpage}

% additional fields for title page
\newcommand*{\organization}[1]{\gdef\@organization{#1}}
\newcommand*{\manuscript}[1]{\gdef\@manuscript{#1}}
\newcommand*{\speciality}[1]{\gdef\@speciality{#1}}
\newcommand*{\degree}[1]{\gdef\@degree{#1}}
\newcommand*{\supervisor}[1]{\gdef\@supervisor{#1}}

\newcommand*{\@organization}[1]{organization is not set}
\newcommand*{\@manuscript}[1]{manuscript is not set}
\newcommand*{\@speciality}[1]{speciality is not set}
\newcommand*{\@degree}[1]{degree is not set}
\newcommand*{\@supervisor}[1]{supervisor is not set}

% title page template
\renewcommand*{\maketitle}{%
\begin{titlepage}
\begin{spacing}{1}
\centering
{\normalsize\@organization\hrule\par}
\vspace{1cm}
{\normalsize\itshape\@manuscript\par}
\vspace{2cm}
{\large\@degree\par}
\vspace{3cm}
{\huge\bfseries\@title\par}
\vspace{1cm}
{\Large\@author\par}
\vspace{3cm}
\begin{flushleft}
{\normalsize\@speciality\par}
\vspace{1cm}
{\normalsize\@supervisor\par}
\end{flushleft}
\vfill
{\normalsize\@date\par}
\end{spacing}
\end{titlepage}
}

% title page numbering
\newcommand{\maketitlepage}{\pagenumbering{gobble}%
\thispagestyle{empty}%
\maketitle%
\clearpage%
\pagenumbering{arabic}%
\setcounter{page}{2}}

\AtBeginDocument{%
\maketitlepage%
}

% referencing figures and tables
\RequirePackage{hyperref}
\RequirePackage[russian]{cleveref}