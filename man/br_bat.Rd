\name{br_bat}
\alias{br_bat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Baseball-Reference MLB Team Standard Batting Table
%%  ~~function to do ... ~~
}
\description{ Scraps MLB Team Fielding Appearances Table from https://www.baseball-reference.com/leagues/MLB/'year'-standard-batting.shtml according to 'year' chosen.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
br_bat(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{ 'year'
  \item{x}{ year
%%     ~~Describe \code{x} here~~
}
}
%%\details{ Provides statistics pertaining to: Tm(Team), #Bat(Number of plaers used in Games), BatAge(Batters' Average Age), R/G(Runs scored per Game), G(Games played or pitched), PA(Plate Appearances), AB(At Bats), R(Runs Scored/Allowed), H(Hits/Hits Allowed), 2B(Doubles Hit/Allowed), 3B(Triples Hit/Allowed), HR(Home Runs Hit/Allowed), RBI(Runs Batted In), SB(Stolen Bases), CS(Caught Stealing), BB(Bases on Balls/Walks), SO(Strikeouts), BA(Hits/At Bats), OBP((H + BB + HBP)/(At Bats + BB + HBP + SF)), SLG(Total Bases/At Bats), OPS(On Base + Slugging Percentages), OPS+(OPS+), TB(Total Bases), GDP(Double Plays Grounded Into), HBP(Times Hit by a Pitch), SH(Sacrifice Hits), SF(Sacrifice Flies), IBB(Intentional Bases on Balls), LOB(Runners Left on Base)
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{A data frame containing a representation of the MLB Team Standard Batting statistics pertaining to the year inputed. For details, please see statistics in https://www.baseball-reference.com/leagues/MLB/2017-standard-batting.shtml
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%%\references{ https://www.baseball-reference.com/leagues/MLB/'year'-standard-batting.shtml
%% ~put references to the literature/web site here ~
%%}
\author{ Henry Alvarez
%%  ~~who you are~~
}
%%\note{ Requires download of package "rvest" since it will be scraping tables from online website.
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Choose year you wish to view
br_bat(2018)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ br_bat }% use one of  RShowDoc("KEYWORDS")
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
