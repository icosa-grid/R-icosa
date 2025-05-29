#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

NumericMatrix rotMat(double theta){
	// Create a numeric matrix
	NumericMatrix mat(2,2);

	// Fill matrix with values
	mat(0,0) = cos(theta);
	mat(0,1) = -sin(theta);
	mat(1,0) = sin(theta);
	mat(1,1) = cos(theta);

	return mat;
}

NumericMatrix mmult( NumericMatrix A , NumericMatrix B) {

	if( A.ncol() != B.nrow() ) stop("Non-conformable arrays") ;

	int rows = A.nrow();
	int cols = B.ncol();
	int matching = A.ncol();

	NumericMatrix out(rows, cols);

	double accumulator =0;
	// go through all the rows
	for (int i = 0; i < rows; i++) {
		for (int j = 0; j < cols; j++) {
			accumulator = 0;
			for(int k = 0; k < matching; k++){
				accumulator += A( i , k) * B(k, j );
			}
			out(i,j) = accumulator;
		}
	}
	return out ;
}


// [[Rcpp::export]]
NumericMatrix rotatePointMatrix_(NumericMatrix coordMat, NumericVector angleVec, NumericVector originVec){

	// ensure deep copy
	NumericMatrix coordMat2 = clone(coordMat);

//	Rcout << "The Coordinates:" << coordMat2 << "\n";

	// location matrix
	for(int i=0; i < coordMat2.nrow(); i++){
		coordMat2(i,0) = coordMat2(i,0) - originVec(0);
		coordMat2(i,1) = coordMat2(i,1) - originVec(1);
		coordMat2(i,2) = coordMat2(i,2) - originVec(2);
	}

//	Rcout << "First" << coordMat2 << "\n";

	// 1. first rotation round x
	// copy second and third column and transpose
	NumericMatrix xMat(2, coordMat2.nrow());

	for(int i=0; i < coordMat2.nrow(); i++){
		xMat(0, i) = coordMat2(i, 1); // copy second column
		xMat(1, i) = coordMat2(i, 2); // copy third column
	}
//	Rcout << "Second" << xMat << "\n";

	NumericMatrix firstRot = rotMat(angleVec(0));

//	Rcout  << "The first rotation matrix:" << firstRot << "\n";

	xMat = mmult(firstRot, xMat);

//	Rcout  << "The first matrix multiplication:" << xMat << "\n";


//	Rcout << "Third" << xMat << "\n";

	for(int i=0; i < coordMat2.nrow(); i++){
		coordMat2(i,1) = xMat(0,i);
		coordMat2(i,2) = xMat(1,i);
	}

	// 2. second rotation
	// copy first and third column and transpose
	NumericMatrix yMat(2, coordMat2.nrow());

	for(int i=0; i < coordMat2.nrow(); i++){
		yMat(0, i) = coordMat2(i, 0); // copy first column
		yMat(1, i) = coordMat2(i, 2); // copy third column
	}

	// matrix multiplication
	NumericMatrix secondRot = rotMat(angleVec(1));
	yMat = mmult(secondRot, yMat);

	// update the first and third
	for(int i=0; i < coordMat2.nrow(); i++){
		coordMat2(i,0) = yMat(0,i);
		coordMat2(i,2) = yMat(1,i);
	}


	// 3. third rotation
	// copy first and second column and transpose
	NumericMatrix zMat(2, coordMat2.nrow());

	for(int i=0; i < coordMat2.nrow(); i++){
		zMat(0, i) = coordMat2(i, 0); // copy first column
		zMat(1, i) = coordMat2(i, 1); // copy third column
	}

	// matrix multiplication
	NumericMatrix thirdRot = rotMat(angleVec(2));
	zMat = mmult(thirdRot, zMat);

	// update the first and second
	for(int i=0; i < coordMat2.nrow(); i++){
		coordMat2(i,0) = zMat(0,i);
		coordMat2(i,1) = zMat(1,i);
	}

	return coordMat2;
}

