\name{make_positivedeviance}
\alias{make_positivedeviance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{make_positivedeviance}
\description{
This function executes an interrupted time series analysis.
}
\usage{
charts(content, topic, outcome, counted, timeperiod, goalu, goall, type, theme)
}
\arguments{
  \item{content}{matrix of data}
  \item{topic}{Title for the plot}
  \item{outcome}{Label for y-axis for the plot, either 'conforming' or 'non-conforming'}
  \item{counted}{If c-chart: events or total}
  \item{timeperiod}{day, week, month, year}
  \item{goalu}{optional horizontal line to indicate the upper boundry of the goal range}
  \item{goall}{optional horizontal line to indicate the lower boundry of the goal range}
  \item{type}{Either: Control chart (proportions), Control chart (counts), Segmented regression (proportions), Segmented regression (counts), Run chart (avoid), Box chart for before-after (avoid)}
  \item{theme}{not implemented}
}
\details{
%%  Names of periods, either numbers, or characters surrounded by single quoates and without embedded commas.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\author{
\itemize{
	\href{https://github.com/orgs/qitools/teams}{qitools}
	}
}
\references{
\itemize{
	\item Taljaard M, McKenzie JE, Ramsay CR, Grimshaw JM. The use of segmented regression in analysing interrupted time series studies: an example in pre-hospital ambulance care. Implement Sci. 2014 Jun 19;9:77. doi: 10.1186/1748-5908-9-77. PMID: \href{http://pubmed.gov/24943919}{24943919}
	\item Auerbach AD, Landefeld CS, Shojania KG. The tension between needing to improve care and knowing how to do it. N Engl J Med. 2007 Aug 9;357(6):608-13. PMID: \href{http://pubmed.gov/17687138}{17687138}.
  }
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
