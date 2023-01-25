
# Spirometry in AfrAm Females ---------------------------------------------
age = 57
height = 190
LLN_PACKAGE = 0.6683429
LLN_GLI = 0.676

## Publication Equations
L = 7.032 - (1.197 * log(age))
M = exp(0.5506 + (-0.1078 * log(height)) + (-0.0544 * log(age)) + 0.0055 - 0.0088)
S = exp(-3.2395 + (0.1850 * log(age)) + 0.0307 - 0.0157)
LLN = exp((log(1 - (1.645 * L * S)) / L) + log(M))

cat("Package: ", LLN_PACKAGE)
cat("GLI: ", LLN_GLI)
cat("Excel: ", LLN)


# Spirometry in AfrAm Females ---------------------------------------------
age = 83
height = 90
LLN_PACKAGE = 0.6552485
LLN_GLI = 0.666

## Publication Equations
L = 7.032 - (1.197 * log(age))
M = exp(0.5506 + (-0.1078 * log(height)) + (-0.0544 * log(age)) + 0.0055 - 0.0266)
S = exp(-3.2395 + (0.1850 * log(age)) + 0.0307 + 0.2188)
LLN = exp((log(1 - (1.645 * L * S)) / L) + log(M))

cat("Package: ", LLN_PACKAGE)
cat("GLI: ", LLN_GLI)
cat("Excel: ", LLN)

# Conclusion: Package predictions match publication formulas and look-up tables. GLI calculator seems to be different.


# DLCO --------------------------------------------------------------------

# GLI calculator supports a max age of 85. However, publication spline tables go up to age 90.
# The provided excel-based macro calculator for DLCO also contains look-up values up to age 90.
# Conclusion: We will retain our calculation of predicted values for the full range of
# ages available in the look-up tables. We will exclude individuals aged > 85 from the
# package testing code as we do not have a GLI calculator comparison which will
# result in always-failing tests.

