# Release Notes  

# denovolyzeR 0.2.0  

- Fixed a critical bug, whereby `denovolyzeByGenes()` returned incorrect output if user overrode default variant classes analysed (using `includeClasses` argument; column labels did not match data content)  
- Updated underlying dependency from reshape to reshape2  
- Changed output format so that all data is returned in columns.  Previously either "gene" or "class" was returned as rownames of the output data structure.  

# denovolyzeR 0.1.1  

- Correct minor errors in vignette  
- Provide citation  
- Address compatibility with updated dplyr 0.5.0

# denovolyzeR 0.1.0  

- This is the first release of denovolyzeR to CRAN  

## pre-release builds (0.0.0.9xxx)  

- 2015-04-01: v0.0.0.9003  
new functionality includes analysis of sub-types of missense variation  
argument names and output formats are about to change: this development release serves to preserce a working version of the package using first draft argument names  

- 2014-11-11: v0.0.0.9000
core statistical functionality is complete and ready for beta-testing  
main limitation is a lack of error-catching  
there may also be very minor modifications to underlying probability tables  
