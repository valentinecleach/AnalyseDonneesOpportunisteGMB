\name{repartition_espece}
\alias{repartition_espece}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
repartition_espece(bdd = Total, nom_ordre, repartition, date_min = params$date_min)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bdd}{
%%     ~~Describe \code{bdd} here~~
}
  \item{nom_ordre}{
%%     ~~Describe \code{nom_ordre} here~~
}
  \item{repartition}{
%%     ~~Describe \code{repartition} here~~
}
  \item{date_min}{
%%     ~~Describe \code{date_min} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
function (bdd = Total, nom_ordre, repartition, date_min = params$date_min) 
{
    bdd_filtre <- bdd \%>\% filter(ordre == nom_ordre, date > params$date_min)
    grps <- fct_lump(bdd_filtre$nom_vernaculaire, prop = 0.03)
    if (length(unique(grps)) > 5) {
        bdd_filtre <- bdd_filtre \%>\% mutate(nom_vernaculaire_grp = grps)
    }
    else {
        bdd_filtre <- bdd_filtre \%>\% mutate(nom_vernaculaire_grp = nom_vernaculaire)
    }
    bdd_filtre \%>\% mutate(nom_vernaculaire_grp = fct_infreq(nom_vernaculaire_grp)) \%>\% 
        ggplot(aes(x = {
            {
                repartition
            }
        }, fill = nom_vernaculaire_grp)) + geom_bar(position = "dodge") + 
        coord_flip() + scale_fill_discrete(labels = label_wrap(40)) + 
        theme_bw()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
