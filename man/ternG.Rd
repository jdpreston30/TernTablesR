% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ternG.r
\name{ternG}
\alias{ternG}
\title{Generate grouped or descriptive summary table with appropriate statistical tests}
\usage{
ternG(
  data,
  vars = NULL,
  exclude_vars = NULL,
  group_var,
  force_ordinal = NULL,
  group_order = NULL,
  descriptive = NULL,
  output_xlsx = NULL,
  output_docx = NULL
)
}
\arguments{
\item{data}{Tibble containing all variables.}

\item{vars}{Character vector of variables to summarize. Defaults to all except `group_var` and `exclude_vars`.}

\item{exclude_vars}{Character vector of variable(s) to exclude. `group_var` is automatically excluded.}

\item{group_var}{Character, the grouping variable (factor or character with >=2 levels).}

\item{force_ordinal}{Character vector of variables to treat as ordinal.}

\item{group_order}{Optional vector to specify custom group level order.}

\item{descriptive}{Logical; if TRUE, suppresses statistical tests and returns descriptive statistics only.}

\item{output_xlsx}{Optional filename for Excel export.}

\item{output_docx}{Optional filename for Word export.}
}
\value{
A tibble with one row per variable (multi-row for multi-level factors), summary statistics by group, and optionally p-value and test type.
}
\description{
Generate grouped or descriptive summary table with appropriate statistical tests
}
