## Test environments
* local macOS 11.4, R 4.0.2
  - 0 errors | 0 warnings | 0 notes
* rhub::check_for_cran(), platform Fedora Linux, R-devel
  - 0 errors | 0 warnings | 2 notes
    ```
    ❯ checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘Matteo Vecchi <matteo.vecchi15@gmail.com>’
  
    New submission
  
     Possibly mis-spelled words in DESCRIPTION:
      Fasta (2:30)

    ❯ checking examples ... NOTE
      Examples with CPU (user + system) or elapsed time > 5s
                   user system elapsed
     auto_match_seqs 7.495  0.181     1.9
    ```
* rhub::check_for_cran(), platform Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  - 0 errors | 0 warnings | 0 note

* rhub::check_for_cran(), platform macOS 10.13.6 High Sierra, R-release, brew
  - 0 errors | 0 warnings | 1 notes
    ```
    ❯ checking CRAN incoming feasibility ... NOTE
     Maintainer: ‘Matteo Vecchi <matteo.vecchi15@gmail.com>’
  
     New submission
    ```

* rhub::check_for_cran(), platform Ubuntu Linux 20.04.1 LTS, R-release, GCC
  - 0 errors | 0 warnings | 1 note
    ```
    ❯ checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘Matteo Vecchi <matteo.vecchi15@gmail.com>’
  
    New submission
  
    Possibly mis-spelled words in DESCRIPTION:
      Fasta (2:30)
    ```
    
    
## Comments
 * rhub checks return only minor notes.
 * implemented comment after first CRAN submission:
   - 1
     ```
     Please write TRUE and FALSE instead of T and F. (Please don't use 'T' or
     'F' as vector names.), e.g.:
       man/get_genbank_table.Rd:
           get_genbank_table(
           df = NULL,
            filename = NULL,
            writetable = F,
            out = "",
            excel.sheet = 1
          )
       man/rename_sequences.Rd:
          rename_sequences(
            fasta_files,
            df = NULL,
            filename = NULL,
            marker_names = NULL,
            out = NULL,
            format = "fasta",
            excel.sheet = 1,
            unalign = F,
            exclude
          )
     ```
     **DONE**
   - 2
        ```
        Please add \value to .Rd files regarding exported methods and explain
        the functions results in the documentation. Please write about the
        structure of the output (class) and also what the output means. (If a
        function does not return a value, please document that too, e.g.
        \value{No return value, called for side effects} or similar)
        Missing Rd-tags:
        example-files-shipped-with-the-package.Rd: \value
        pipe.Rd: \arguments,  \value
        rename_sequences.Rd: \value
        ```
     **DONE:** : Added a \value tag for rename_sequences(). 
                Improved the import of magrittr's pipe so that it is properly documented. 
                Added a function concatipede_example() to document in a clean way the example files shipped with the package.
    
   - 3
        ```
        Please always make sure to reset to user's options(), working directory
        or par() after you changed it in examples and vignettes and demos.
        e.g.:
        oldpar <- par(mfrow = c(1,2))
        ...
        par(oldpar)
        
        old <- options(digits = 3)
        ...
        options(old)
        
        oldwd <- getwd()
        ...
        setwd(oldwd)
        
        e.g.: tests and vignette
        ```
    **DONE:** : Double-checked that the initial working directory was restored at the end of the tests. 
                Made sure that options(), working directory, and par() are restored in the vignette

