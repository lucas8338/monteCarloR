#include <string.h>
#include "sumIntArray.c"
#include "accessStringArrayByRowAndCol.c"

/*
    the objective of this script is to count how much time a row has repeated
    in the data.
    @param toMatch: a string array where each value is a value of the row otherwise
    each value is the value of the next column at the same row.
    @param *data[]: a array of strings is the data.frame flattened.
    @param int *data
*/
int countNOccurrencesOfARowString(char *uniques[], char *data[], int nrow, int ncol){
    int result=0;
    int rowResult[ncol];
    for ( int i=0; i<nrow; i++ ){
        for ( int j = 0; j<ncol; j++){
            char *actualStr = accessStringArrayByRowAndCol(data, nrow, i+1, j+1);
            if ( strcmp(uniques[j], actualStr ) == 0 ){
                rowResult[j] = 1;
            }else{
                rowResult[j] = 0;
            };
        };
        int countOnes = sumIntArray(rowResult, ncol);
        if ( countOnes == ncol ){
            result = result + 1;
        };
    };
    return result;
}

