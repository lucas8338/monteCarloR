#include "include/myself/countNOccurrencesOfARowString.c"


/*
    a function to R which will check how much a row happens in a data.frame.
    this is made to work only with strings.
    @param *uniques[]: a character array the uniques values to use. should
    not have repeated values each row must be unique cause this function
    will count how much times each row of uniques happened in data.
    @param *data[]: a character array the complete data.frame to calculate
    the values of repeated "uniques", the ncol of the data.frame which generated
    this must be equal to the unique, cause the ncol parameter will be used for
    both them.
    @param *nrow: the number of rows of the data data.frame.
    @param *ncol: the number of columns of the uniques and data data.frame.
    @param *out: the output is the parameter to be modified by this function
    and will be used as result of this function by R language.
*/
void RC_countNOccurrencesOfEachRowString(char **uniques, char **data, int *uniques_nrow, int *data_nrow, int *ncol, int *out){
    int result[uniques_nrow[0]];
    for ( int i=0; i<uniques_nrow[0]; i++ ){
        // "actualRow" store the number of the actual row (starting is 1).
        int actualRow = i + 1;
        // "actualUniques" is a array with all values of the actual row.
        char *actualUniques[ncol[0]];
        for ( int j=0; j<ncol[0]; j++ ){
            int actualCol = j + 1;
            actualUniques[j] = accessStringArrayByRowAndCol(uniques, uniques_nrow[0], actualRow, actualCol);
        };
        int actualRowHappenedCount = countNOccurrencesOfARowString(actualUniques, data, data_nrow[0], ncol[0]);
        result[i] = actualRowHappenedCount;
    };
    
    *out = *result;
    /* i dont know why but the loop bellow is required to fix a bug, when i was
    ending this function on the line above only, in my test only the first value
    of the variable "out" was being correctly set, but the second (the test was 
    2 uniques row) was not being changed, but when i add the row bellow all worked
    fine, and a bug happens if a remove the row above, so i'm keeping both. */
    for ( int i=0; i<uniques_nrow[0]; i++ ){
       out[i] = result[i];
    };
}