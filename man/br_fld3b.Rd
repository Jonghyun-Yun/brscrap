\name{br_fld3b}
\alias{br_fld3b}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Baseball-Reference MLB Team Fielding - Third Base Table
%%  ~~function to do ... ~~
}
\description{ Scraps MLB Team Fielding - Third Base Table from https://www.baseball-reference.com/leagues/MLB/'year'-specialpos_3b-fielding.shtml according to 'year' chosen.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
br_fld3b(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{ 'year'
  \item{x}{ year
%%     ~~Describe \code{x} here~~
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{ A data frame containing a representation of the MLB Team Fielding - Third Base Table statistics pertaining to the year inputed. For details, please see statistics in https://www.baseball-reference.com/leagues/MLB/2017-specialpos_3b-fielding.shtml
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{ Henry Alvarez
%%  ~~who you are~~
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
## Choose year you wish to view
br_fld3b(2018)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ br_fld3b }% use one of  RShowDoc("KEYWORDS")
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
