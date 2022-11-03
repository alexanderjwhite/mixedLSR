## Test environments

* Windows Server 2022, R-devel, 64 bit

## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new release. Github actions R CMD checks return with no errors, warnings, or notes.
* devtools::check_rhub() returned no ERRORs or WARNINGs. There are two notes:

```
Possibly misspelled words in DESCRIPTION:
  mixedLSR (10:67, 11:50)
```

* This is spelled as intended.

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```

* As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.



  
