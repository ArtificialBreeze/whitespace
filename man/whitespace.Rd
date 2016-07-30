\name{whitespace}
\alias{whitespace}
\title{Whitespace compiler for R}
\usage{
dfa()
parse()
compile()
}
\description{
Formats the code and creates a Deterministic Finite-State automaton (DFA)
Parses the code
Compiles and runs the code
}
\examples{
compile(parse(dfa("   \t\t\t\t\t\t\n\t\n  \n\n\n")))
# Should print:
# Program output:  ?
# Program stop
}
