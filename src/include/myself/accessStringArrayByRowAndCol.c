/*
    the function bellow has the objective to acess a array (flattened array) as it were
    a data.frame, acess it using the row number and the col number
    @param **data: is a array, infortunely C only accps one data, so *data is one type array
    for example: array of string (char).
    @param data_nrow: is the number of rows which the data has it will be used to get the correct wanted value.
    @param wanted_row: is the number of the row which you want to return it start from 1.
    @param wanted_col: is the number of the row which you want to return it start from 1.
    @examples:
        // bellow is a example of a script executable
        #include <stdio.h>
        #include "accessStringArrayByRowAndCol.c"

        int main(){
            char *data[]={"tala","tara","fsed","fsfes","fsefs","efs"};
            char *result=accessStringArrayByRowAndCol(data, 2, 2, 3);
            printf("%s", result);
            return 0;
        }
*/
char *accessStringArrayByRowAndCol(char *data[], int data_nrow, int wanted_row, int wanted_col){
    if ( wanted_row > data_nrow ){
        return "error: wanted_row cant be bigger than data_nrow";
    };
    if ( wanted_col == 1 ){
        return data[wanted_row - 1];
    }else if ( wanted_col > 1 ){
        int realyWanted = wanted_row - 1 + wanted_col * data_nrow - data_nrow;
        return data[realyWanted];
    }else{
        return "error: nothing was found, check the parameters.";
    };
}