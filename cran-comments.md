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
   -   ``` Please write TRUE and FALSE instead of T and F. (Please don't use 'T' or
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



