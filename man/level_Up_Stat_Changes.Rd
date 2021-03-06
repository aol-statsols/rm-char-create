% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/level_Up_Stat_Changes.R
\name{level_Up_Stat_Changes}
\alias{level_Up_Stat_Changes}
\title{A STAT FUNCTION}
\usage{
level_Up_Stat_Changes(stat.Tmp = -1, stat.Pot = -1, char.Age = -1,
  char.Age_Die = -1, species.Middle_Aged = -1, species.Old = -1,
  species.Venerable = -1, age_Crisis.Middle_Aged = -1,
  age_Crisis.Old = -1, age_Crisis.Venerable = -1)
}
\arguments{
\item{stat.Tmp}{CHARACTER'S CURRENT TEMPORARY STATS}

\item{stat.Pot}{CHARACTER'S CURRENT POTENTIAL STATS}

\item{char.Age}{CHARACTER'S CURRENT AGE}

\item{char.Age_Die}{CHARACTER'S AGE DIE}

\item{species.Middle_Aged}{WHEN MIDDLE AGE HAPPENS FOR CHARACTER'S SPECIES}

\item{species.Old}{WHEN OLD AGE HAPPENS FOR CHARACTER'S SPECIES}

\item{species.Venerable}{WHEN VENERABLE AGE HAPPENS FOR CHARACTER'S SPECIES}

\item{age_Crisis.Middle_Aged}{HAS MIDDLE AGE CRISIS HAPPENED FOR CHARACTER'S SPECIES}

\item{age_Crisis.Old}{HAS OLD AGE CRISIS HAPPENED FOR CHARACTER'S SPECIES}

\item{age_Crisis.Venerable}{HAS VENERABLE AGE CRISIS HAPPENED FOR CHARACTER'S SPECIES}
}
\description{
CHANGES STATS AT EACH LEVEL, INCLUDING CHECKING FOR AGING CRISES
}
\examples{
level_Up_Stat_Changes()
}
\concept{
stat stats
}
