#include <RcppArmadillo.h>
using namespace Rcpp;
//[[Rcpp::depends(RcppArmadillo)]]

//This is to access the print function.
// It is not available outside of Rcpp!
extern "C" SEXP fwriteMain(SEXP MAT,   //matrix test
                          SEXP filename_Arg,
                          SEXP sep_Arg,
                          SEXP eol_Arg,
                          SEXP dec_Arg,
                          SEXP buffMB_Arg); // [1-1024] default 8MB


//' @export
// [[Rcpp::export]]
void Split_Aggregate(String& maleFile, String& femaleFile, String eol,
                     NumericMatrix& maleMat, NumericMatrix& femaleMat){
  
  // Rcpp::Rcout << "I'm in" << std::endl;
  // 
  // IntegerVector intPatches(maleMat(_,1).begin(), maleMat(_,1).end());
  // IntegerVector uniquePatches = unique(intPatches);
  // int totalTime = maleMat.nrow();
  // int totalPatch = uniquePatches.length();
  // 
  // std::sort(uniquePatches.begin(), uniquePatches.end());
  // 
  // Rcpp::Rcout << "I got unique patches" << std::endl;
  // 
  // String patchName;
  // std::string new_string;
  // int patchTime = totalTime/totalPatch;
  // NumericMatrix hold(patchTime, maleMat.ncol());
  // colnames(hold) = colnames(maleMat);
  // 
  // Rcpp::Rcout <<"I have male patch things" << std::endl;
  // 
  // int step = maleMat.ncol()-2;
  // int whichRow, subset, duplicatedGens=2, nextStep=2+step, uniqueGens=2;
  // 
  // int finalStep = maleMat.ncol()-1;
  String comma = ",";
  String dot = ".";
  int buffMem = 8;
  // 
  // 
  // /*
  // * aggregate females first, then use 1 loop for subsetting
  // */
  // NumericMatrix femaleAgg(totalTime,maleMat.ncol());
  // femaleAgg(_,0) = maleMat(_,0);
  // femaleAgg(_,1) = intPatches;
  // 
  // Rcpp::Rcout << "I made it here!" << std::endl;
  // 
  // //aggregate females
  // Rcpp::Rcout << "uniqueGens: " << uniqueGens << std::endl;
  // Rcpp::Rcout << "finalStep: " << finalStep << std::endl;
  // Rcpp::Rcout << "step: " << step << std::endl;
  // 
  // 
  // for(uniqueGens; uniqueGens<=finalStep; ++uniqueGens, nextStep+=step){
  //   //overwrite values in hold with female values
  //   femaleAgg(_,uniqueGens) = femaleMat(_,duplicatedGens);
  //   //duplicatedGens++;
  //   
  //   //aggregate females over their mate genotypes
  //   for(duplicatedGens++; duplicatedGens<nextStep; duplicatedGens++){
  //     femaleAgg(_,uniqueGens) = femaleAgg(_,uniqueGens) + femaleMat(_,duplicatedGens);
  //   }//genotype sum loop
  //   
  // }//end full aggregate loop
  // 
  // Rcpp::Rcout << "I aggregated" << std::endl;
  // 
  // //loop over patches
  // for(int patch : seq(0, totalPatch-1)){
  //   
  //   patchName = maleFile;
  //   whichRow = uniquePatches[patch];
  //   new_string = std::string(5 - toString(whichRow).length(), '0') + toString(whichRow);
  //   patchName.replace_last(".csv", "_Patch"+new_string+".csv");
  //   
  //   
  //   whichRow = patch;
  //   
  //   //subset by patch
  //   for(subset=0; subset<patchTime; ++subset, whichRow+=totalPatch){
  //     hold(subset,_) = maleMat(whichRow,_);
  //   }//end subset loop
  //   
  //   //write male output
  //   fwriteMain(wrap(hold), wrap(patchName), wrap(comma), wrap(eol),
  //              wrap(dot), wrap(buffMem));
  //   
  //   
  //   //subset by patch - females
  //   whichRow = patch;
  //   for(subset=0; subset<patchTime; ++subset, whichRow+=totalPatch){
  //     hold(subset,_) = femaleAgg(whichRow,_);
  //   }//end subset loop
  //   
  //   //Write female output
  //   patchName = femaleFile;
  //   patchName.replace_last(".csv", "_Aggregate_Patch"+new_string+".csv");
    
    fwriteMain(wrap(maleMat), wrap(maleFile), wrap(comma), wrap(eol),
               wrap(dot), wrap(buffMem));
    
//  }//end patch loop
  
}//end Split_Aggregate


//' @export
// [[Rcpp::export]]
void Mean_Quantiles(CharacterVector& maleNames, CharacterVector& femaleNames,
                    bool& doMean, bool& doQuant, NumericVector& quantiles,
                    CharacterVector& colNames, String& eol,
                    arma::Cube<arma::uword>& maleData,
                    arma::Cube<arma::uword>& femaleData){
  
  //setup print stuff
  String comma = ",";
  String dot = ".";
  int buffMem = 8;
  
  //setup print matrix, put in time column, give it column names
  NumericMatrix printThing(maleData.n_rows, maleData.n_slices+1);
  printThing(_,0) = seq_len(maleData.n_rows);
  colnames(printThing) = colNames;
  
  //holder vector to pull things out of arma cubes
  arma::dvec holder(maleData.n_rows, arma::fill::zeros);
  
  //counter used in both sections, define and fill here.
  IntegerVector sliceSeq(seq(0, maleData.n_slices-1));
  
  
  if(doMean){
    //Loop over slices - male stuff
    for(int slice : sliceSeq){
      //get slice, do row means
      holder = arma::conv_to< arma::dvec >::from(arma::mean(maleData.slice(slice), 1));
      //put row means into print matrix
      printThing(_,slice+1) = NumericVector(holder.begin(), holder.end());
    }
    
    //Print male output!
    fwriteMain(wrap(printThing), wrap(String(maleNames[0]) ),
               wrap(comma), wrap(eol), wrap(dot), wrap(buffMem));
    
    //Loop over slices - female stuff
    for(int slice : sliceSeq){
      //get slice, do row means
      holder = arma::conv_to< arma::dvec >::from(arma::mean(femaleData.slice(slice), 1));
      //put row means into print matrix
      printThing(_,slice+1) = NumericVector(holder.begin(), holder.end());
    }
    
    //Print female output
    fwriteMain(wrap(printThing), wrap(String(femaleNames[0]) ),
               wrap(comma), wrap(eol), wrap(dot), wrap(buffMem));
    
  }//end mean
  
  
  
  if(doQuant){
    //Set error for rounding issues
    double fuzz = 4.0*std::numeric_limits<double>::epsilon();
    
    //length of input vector
    int vecLen = maleData.n_cols;
    
    //Things used inside loop
    double nppm, h;
    int j;
    IntegerVector holdRow(vecLen), test(vecLen+2);
    IntegerVector rowSeq(seq(0, maleData.n_rows-1));
    
    
    //loop over quantiles to calculate
    for(double whichQuant : seq(0, quantiles.length()-1) ){
      
      //1/3 and 2/3 are to match algorithm 8 in the rstats quantile function
      //Not really sure what else is happening. I took this from R quantile function
      nppm = 1.0/3.0 + quantiles[whichQuant]*(vecLen + 1.0/3.0); //remove part of this to other loop?
      h = nppm + fuzz;
      j = int(h);
      h = nppm - j;
      
      //Safety to set small values of h to 0. This does not handle negative values
      // like the r quantile does.
      if(h<fuzz) h = 0;
      
      
      //loop over slices - do males
      for(int slice : sliceSeq){
        //Loop over rows, calculate quantiles in each row
        for(int currentRow : rowSeq){
          
          //subset matrix. I use several times
          holder = arma::conv_to< arma::dvec >::from(maleData.slice(slice).row(currentRow));
          holdRow = NumericVector(holder.begin(), holder.end());
          
          //sort vector
          std::sort(holdRow.begin(), holdRow.end());
          
          //Set vector with row, and an extra min/max. This is part of the formula,
          // It has been adapted here to avoid extra copies from push_front/bach
          test[0]=holdRow[0];
          test[seq(1,vecLen)] = holdRow;
          test[vecLen+1]=holdRow[vecLen-1];
          
          //calculate quantiles. Also not sure why or how this works.
          printThing(currentRow,slice+1) = (1-h)*test[j] + h*test[j+1];
          
        }//end loop over rows
      }// end loop over slice
      
      //Print out male stuff
      fwriteMain(wrap(printThing), wrap(String(maleNames[whichQuant+1])),
                 wrap(comma), wrap(eol), wrap(dot), wrap(buffMem));
      
      
      //loop over slices - do females
      for(int slice : sliceSeq){
        //Loop over rows, calculate quantiles in each row
        for(int currentRow : rowSeq){
          
          //subset matrix. I use several times
          holder = arma::conv_to< arma::dvec >::from(femaleData.slice(slice).row(currentRow));
          holdRow = NumericVector(holder.begin(), holder.end());
          
          //sort vector
          std::sort(holdRow.begin(), holdRow.end());
          
          //Set vector with row, and an extra min/max. This is part of the formula,
          // It has been adapted here to avoid extra copies from push_front/bach
          test[0]=holdRow[0];
          test[seq(1,vecLen)] = holdRow;
          test[vecLen+1]=holdRow[vecLen-1];
          
          //calculate quantiles. Also not sure why or how this works.
          printThing(currentRow,slice+1) = (1-h)*test[j] + h*test[j+1];
          
        }//end loop over rows
      }// end loop over slice
      
      //Print out female stuff
      fwriteMain(wrap(printThing), wrap(String(femaleNames[whichQuant+1])),
                 wrap(comma), wrap(eol), wrap(dot), wrap(buffMem));
      
    }//end loop over quantiles
  }//end quantiles chunk
  
}//end analysis function