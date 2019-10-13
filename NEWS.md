metacom 1.5.2
==============

* Order the null matrices for Coherence, and allow option to order them for turnover calculation. Not ordering nulls for Coherence results in erroneous positive coherence.


metacom 1.5.1
==============

* Do not ordinate the null matrices. This won't affect the results from fixed-fixed null.



metacom 1.5.0
==============

* Implemented the original null model for turnover as outlined in Leibold and Mikkelson 2002. Previous versions used a more traditional null model approach than shifting species filled ranges across sites as a block. There are pros and cons to both methods, but now the user has a choice.
