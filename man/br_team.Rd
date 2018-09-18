\name{br_team}
\alias{br_team}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Baseball-Reference MLB Team Statistics Wrapper
%%  ~~function to do ... ~~
}
\description{ Wraps values of br_bat(), br_pitch(),br_bat_pitch(), and br_stand() tables into one output pertaining to 'year' or 'years' chosen.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
br_team(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{ year, years
  \item{x}{ 'year' or 'years'
%%     ~~Describe \code{years} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{ A list of data frames containing statistics pertaining to the functions br_bat(), br_pitch(), br_bat_pitch(), and br_stand() pertaining to the singular year or range of years chosen. For more details refer to the mentioned functions.
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
## Choose year or range of years you wish to view
br_stand(2018)
or
br_stand(2015:2018)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ br_team }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
