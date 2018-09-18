\name{br_app}
\alias{br_app}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Baseball-Reference MLB Team Fielding Appearances Table
%%  ~~function to do ... ~~
}
\description{ Scraps MLB Team Fielding Appearances Table from  https://www.baseball-reference.com/leagues/MLB/'year'-appearances-fielding.shtml according to `year` chosen.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
br_app(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{'year'
  \item{x}{ year
%%     ~~Describe \code{x} here~~
}
}
%% \details{Provides statistics pertaining to: Tm(Teams), G(All Games Played), GS(Games Started), Batting(Games appeared in the batting order, but may not have batted), Defense(Games in lineup at a defensive position), P(Games in lineup or announced as a pitcher), C(Games in lineup as a Catcher), 1B(Games in lineup as a First Baseman), 2B(Games in lineup as a Second Baseman), 3B(Games in lineup as a Third Baseman), SS(Games in lineup as a Shortstop), LF(Games in lineup as a Left Fielder), CF(Games in lineup as a Center Fielder), RF(Games in lineup as a Right Fielder), OF(Games in lineup as a Outfielder), DH(Games in lineup as a Designated Hitter), PH(Games in lineup as a Pinch Hitter, may have played another position as well), PR(Games in lineup as a Pinch Runner, may have played another position as well).
%%  ~~ If necessary, more details than the description above ~~

\value{ A data frame containing a representation of the MLB Team Fielding Appearances statistics pertaining to the year inputed. For details, please see statistics in https://www.baseball-reference.com/leagues/MLB/2017-appearances-fielding.shtml
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{ https://www.baseball-reference.com/leagues/MLB/'year'-appearances-fielding.shtml
%% ~put references to the literature/web site here ~

\author{ Henry Alvarez
%%  ~~who you are~~
}
%\note{ Requires download of package "rvest" since it will be scraping tables from online website.
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Choose year you wish to view
br_app(2018)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ br_app }% use one of  RShowDoc("KEYWORDS")

