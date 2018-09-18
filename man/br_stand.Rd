\name{br_stand}
\alias{br_stand}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Baseball-Reference MLB Team Standings Table
%%  ~~function to do ... ~~
}
\description{ Scraps MLB Team Standings Table from https://www.baseball-reference.com/leagues/MLB/'year'-standings.shtml according to `year` chosen.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
br_stand(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{ year
  \item{x}{ 'year'
%%     ~~Describe \code{year} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{ A data frame containing a representation of the MLB Team Standings statistics pertaining to the year inputed while supplying a column with post season playoff and league win information. For details, please see statistics in https://www.baseball-reference.com/leagues/MLB/2017-standings.shtml
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{ Jonghyun Yun
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Choose year you wish to view
br_stand(2018)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ br_stand }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
