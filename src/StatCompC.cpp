#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

//' @title bubble sort algorithm using Rcpp
//' @description bubble sort algorithm using Rcpp
//' @param arr Array to be sorted
//' @return the sorted array
//' @examples
//' \dontrun{
//' dt <- c(1,4,6,3,2);
//' bubbleSortCpp(dt);
//' }
//' @export
// [[Rcpp::export]]
NumericVector bubbleSortCpp(NumericVector arr){
  double tem = 0;
  if(arr.size()<2){
    return arr;
  }
  int n = arr.size();
  NumericVector cloArr = clone(arr);
  for(int i = n-1; i>0;i--){
    for(int j = 0; j < i; j++){
      if(arr[j]>arr[j+1]){
        tem = arr[j];
        arr[j] = arr[j+1];
        arr[j+1] = tem;
        cloArr[j] = cloArr[j+1];
        cloArr[j+1] = tem;
      }
    }
  }
  return cloArr;
}