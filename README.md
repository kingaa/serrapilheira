Inference for Disease Dynamics<br>Course Materials
--------------------------------------------------

### Required software

The codes require, at a minimum, [**R**](https://cran.r-project.org/)
version 4.2 and [**pomp**](https://kingaa.github.io/pomp/) version 5.2.
Windows users must also have the appropriate version of
[**Rtools**](https://cran.r-project.org/bin/windows/Rtools/) installed.
The `prep` directory contains scripts that will install other needed
packages and test the user’s installation.

A [Github Action](https://github.com/kingaa/serrapilheira/actions)
checks that these installations and actions succeed on a variety of
current and legacy platforms:

[![install-test](https://github.com/kingaa/serrapilheira/actions/workflows/install-test.yml/badge.svg)](https://github.com/kingaa/serrapilheira/actions/workflows/install-test.yml)

------------------------------------------------------------------------

### Compilation

Full compilation of all the materials can be accomplished by running
`make` in the root directory. This requires substantial resources. For
this reason, the most expensive computations are archived using the
facilities provided for the purpose in **pomp**. Compilation with these
archives in place requires much less time than does compilation from
scratch. The following gives an indication of the size of the archives
and the time required for their computation on a linux cluster with
250 cpus.

<table>
<thead>
<tr class="header">
<th style="text-align: left;">directory</th>
<th style="text-align: right;">size (kB)</th>
<th style="text-align: right;">time (min)</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">measles/results</td>
<td style="text-align: right;">340</td>
<td style="text-align: right;">219.3</td>
</tr>
<tr class="even">
<td style="text-align: left;">mif/results</td>
<td style="text-align: right;">2244</td>
<td style="text-align: right;">67.2</td>
</tr>
<tr class="odd">
<td style="text-align: left;">pfilter/results</td>
<td style="text-align: right;">142</td>
<td style="text-align: right;">18.9</td>
</tr>
<tr class="even">
<td style="text-align: left;">ebola/results</td>
<td style="text-align: right;">2259</td>
<td style="text-align: right;">0.4</td>
</tr>
</tbody>
</table>

The archives amount to about 5 MB.

Full compilation, i.e., rebuilding the complete set of materials
following deletion of all archives, requires about 321 min on a
250-processor cluster. Full compilation regenerates the complete set of
archives. A finishing compilation, i.e., rebuilding with all archives in
place, but with the re-Making of all documents, requires about 12 min on
a 64-cpu workstation.

------------------------------------------------------------------------

### License

[![CC-BY-NC](https://i.creativecommons.org/l/by-nc/4.0/88x31.png)](https://creativecommons.org/licenses/by-nc/4.0/)

This work is licensed under the [Creative Commons
Attribution-NonCommercial 4.0 International License (CC BY-NC
4.0)](https://creativecommons.org/licenses/by-nc/4.0/). Under its terms,
you are free to:

-   Share — copy and redistribute the material in any medium or format
-   Adapt — remix, transform, and build upon the material

under the following terms:

-   Attribution — You must give appropriate credit, provide a link to
    the license, and indicate if changes were made. You may do so in any
    reasonable manner, but not in any way that suggests the licensor
    endorses you or your use.
-   NonCommercial — You may not use the material for commercial
    purposes.
-   No additional restrictions — You may not apply legal terms or
    technological measures that legally restrict others from doing
    anything the license permits.

The licensor cannot revoke these freedoms as long as you follow the
license terms.

------------------------------------------------------------------------
