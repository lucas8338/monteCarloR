/*
    a test to test the expected result from the C R wrapper function
    "RC_countNOccurrencesOfEachRowString.c", compile this file then run it
    it should to print "2" "1", each number in a row.
*/

#include <stdio.h>
#include "../RC_countNOccurrencesOfEachRowString.c"

int main(){
    char *data[9]={"foo","bar","foo","foo","poing","foo","foo","bar","foo"};
    char *uniques[6]={"foo","bar","foo","poing","foo","bar"};
    int uniques_nrow[1]={2};
    int data_nrow[1]={3};
    int ncol[1]={3};
    int output[2]={0,0};
    RC_countNOccurrencesOfEachRowString(uniques, data, uniques_nrow, data_nrow, ncol, output);
    printf("%i\n", output[0]);
    printf("%i\n", output[1]);
    return 0;
}