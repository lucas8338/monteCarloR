/*
    a function to sum the values of a int array
    @param data[]: a int array.
    @param length: the size of the array data[].
*/
int sumIntArray(int data[], int length){
    int result = 0;
    for ( int i=0; data[i] <= length ; i++ ){
        result = result + data[i];
    };
    return result;
}