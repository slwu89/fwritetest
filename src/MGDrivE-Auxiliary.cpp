







#include "MGDReader.h"
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
Rcpp::NumericMatrix testRead(const std::string& fileName, const int& simTime, const int& numPatch,
                             const Rcpp::CharacterVector& genotypes){
  
  Rcpp::Rcout << "Input file is: " << fileName << std::endl;
  
  Rcpp::Rcout << "file name length: " << fileName.size() << std::endl;
  Rcpp::Rcout << "genotypes length: " << genotypes.size() << std::endl; 
  
  
  
  Rcpp::NumericMatrix testMat(simTime*numPatch, genotypes.size()+2);
  Rcpp::CharacterVector cNames(genotypes);
  
  cNames.push_front("Patch");
  cNames.push_front("Time");
  
  
  colnames(testMat) = cNames;
  
  
  
  MGDReader fileReader(fileName);
  
  
  
  
  fileReader.readFileDBL(fileName, simTime*numPatch, genotypes.size()+2, testMat);
  
  
  return testMat;
  
}











//' @export
// [[Rcpp::export]]
void S_A(const std::vector<std::string>& outputFiles,
                     const std::vector<std::string>& maleFiles, const std::vector<std::string>& femaleFiles,
                     const int& simTime, const int& numPatch, const Rcpp::CharacterVector& genotypes){
  
  /********************
   * setup objects for every run
   ********************/
  
  // setup basic objects from input
  IntegerVector patches = seq(0, numPatch-1);
  int patchTime = simTime-1; // minus one because printing is 1-(end-1)
  int step = genotypes.size();
  int finalStep = genotypes.size() + 1;
  
  // setup matrix for holding/printing
  CharacterVector cNames(genotypes);
  cNames.push_front("Time");
  NumericMatrix hold(patchTime, genotypes.size()+1);
  colnames(hold) = cNames;
  hold(_,0) = seq(1, patchTime);

  // Matrices for holding data during execution
  NumericMatrix maleMat(numPatch*patchTime, step+2);
  NumericMatrix femaleMat(numPatch*patchTime, step*step+2);
  NumericMatrix femaleAgg(numPatch*patchTime, step+2);

  // things used later in the loops
  std::string patchName;
  std::string new_string;
  int whichRow, subset, duplicatedGens, nextStep, whichCol;

  
  /********************
   * initialize reader
   ********************/
  
  MGDReader fileReader(femaleFiles[0]);

  
  /********************
   * Loop over files and do work
   ********************/
  
  for(int file=0; file<femaleFiles.size(); file++){
    
    // read male file
    fileReader.readFileDBL(maleFiles[file], numPatch*patchTime, step+2, maleMat);
    
    // read female file
    fileReader.readFileDBL(femaleFiles[file], numPatch*patchTime, step*step+2, femaleMat);
    
    
    
    
    // aggregate females
    duplicatedGens=2;
    nextStep = step + 2;
    for(whichCol=2; whichCol<=finalStep; ++whichCol, nextStep+=step){
      
      //overwrite values in hold with female values
      femaleAgg(_,whichCol) = femaleMat(_,duplicatedGens);
      
      //aggregate females over their mate genotypes
      for(duplicatedGens++; duplicatedGens<nextStep; duplicatedGens++){
        femaleAgg(_,whichCol) = femaleAgg(_,whichCol) + femaleMat(_,duplicatedGens);
      }//genotype sum loop
      
    }//end full aggregate loop
    
    
    
    //loop over patches
    for(int patch : patches){
      
      // generate patch portion of file name
      new_string = std::string(4 - toString(patch).length(), '0') + toString(patch);
      
      /********************
       * Male Patch Subset
       ********************/
      
      // generate file name
      patchName = outputFiles[file];
      patchName += "/ADM_Patch" + new_string + ".csv";
      
      //subset by patch
      whichRow = patch;
      for(subset=0; subset<patchTime; ++subset, whichRow+=numPatch){
        for(whichCol=2; whichCol<finalStep; ++whichCol){
          hold(subset,whichCol-1) = maleMat(whichRow,whichCol);
        }
      }//end subset loop
      
      
      //write male output
      fwriteMain(wrap(hold), wrap(patchName), wrap(","), wrap("\n"),
                 wrap("."), wrap(8));
      
      
      /********************
       * Female Patch Subset
       ********************/
      
      // female file name
      patchName = outputFiles[file];
      patchName += "/AF1_Aggregate_Patch" + new_string + ".csv";
      
      //subset by patch
      whichRow = patch;
      for(subset=0; subset<patchTime; ++subset, whichRow+=numPatch){
        for(whichCol=2; whichCol<finalStep; ++whichCol){
          hold(subset,whichCol-1) = femaleAgg(whichRow,whichCol);
        }
      }//end subset loop
      
      // write output
      fwriteMain(wrap(hold), wrap(patchName), wrap(","), wrap("\n"),
                 wrap("."), wrap(8));
      
    }//end patch loop
    
  } // end loop over files
  
}//end Split_Aggregate

























//' @export
// [[Rcpp::export]]
void M_Q(const std::string& writeDir, const Rcpp::ListOf<Rcpp::List>& inputFiles,
                    const bool& doMean, const bool& doQuant, const std::vector<double>& quantiles, const int& numReps,
                    const int& simTime, const int& numPatch, const Rcpp::CharacterVector& genotypes){
  
  
  /********************
   * setup objects for every run
   ********************/
  
  // setup basic objects from input
  IntegerVector patches = seq(0, numPatch-1);
  IntegerVector sliceSeq(seq(1, genotypes.size()));
  int patchTime = simTime-1; // minus one because printing is 1-(end-1)
  int numCols = genotypes.size()+1;
  
  
  // setup matrix for printing
  CharacterVector cNames(genotypes);
  cNames.push_front("Time");
  NumericMatrix printThing(patchTime, numCols);
  colnames(printThing) = cNames;
  printThing(_,0) = seq(1, patchTime);
  std::vector<std::string> sexNames{"/ADM_", "/AF1_"};
  
  
  // data holders
  arma::Cube<int> inputData(patchTime, numReps, numCols, arma::fill::zeros);

  
  // things used later in the loops
  arma::dvec holder(patchTime, arma::fill::zeros);
  arma::Mat<int> readHold(patchTime, numCols);
  std::string patchName;
  std::string new_string;
  
  
  
  /********************
   * quantiles stuff used every run
   ********************/
  double fuzz = 4.0*std::numeric_limits<double>::epsilon(); //Set error for rounding issues
  IntegerVector numQuants;
  std::vector<std::string> quantNames(quantiles.size());

  if(doQuant){
    
    numQuants = seq(0, quantiles.size()-1);

    // convert quantiles to strings for naming files later
    for(int i : numQuants){
      quantNames[i] = std::to_string(quantiles[i]);
    }
  }//end if
  
  //Things used inside quantile loop
  double nppm, h;
  int j;
  IntegerVector holdRow(numReps), test(numReps+2);
  IntegerVector rowSeq(seq(0, patchTime-1));
  
  
  /********************
   * initialize reader
   ********************/
  
  MGDReader fileReader(Rcpp::as<std::vector<std::string> >(inputFiles[0][0])[0]);
  
  
  /********************
   * Actual work begins with this loop
   ********************/
  
  // loop over list of lists for input file. This does all male/female files
  for(int sex : {0,1}){
      
    /********************
     * Loop over patches and do work
     ********************/
    
    for(int patch : patches){
      
      // generate patch portion of file name
      new_string = std::string(4 - toString(patch).length(), '0') + toString(patch);
      
      // read in Data
      for(int whichRep=0; whichRep<numReps; whichRep++){
        
        fileReader.readFileINT(Rcpp::as<std::vector<std::string> >(inputFiles[sex][whichRep])[patch],
                               patchTime, numCols, readHold);
        inputData.tube(0,whichRep,patchTime-1,whichRep) = readHold;  
              
      }
      
      
      /********************
       * Mean
       ********************/
      
      if(doMean){
        //Loop over slices
        for(int slice : sliceSeq){
          
          //get slice, do row means
          // awful conversion. for some reason, providing ints to mean returns an integer result. wtf?
          holder = arma::conv_to< arma::dvec >::from(arma::mean(arma::conv_to< arma::Mat<double> >
                                                                  ::from(inputData.slice(slice)), 1));
          //put row means into print matrix
          printThing(_,slice) = NumericVector(holder.begin(), holder.end());
          
        }//end loop over slices
        
        // generate file name
        patchName = writeDir + sexNames[sex] + "Mean_Patch" + new_string + ".csv";
        
        //Print male output!
        fwriteMain(wrap(printThing), wrap(patchName),
                   wrap(","), wrap("\n"), wrap("."), wrap(8));
        
      }//end mean
      
      
      
      /********************
       * quantiles
       ********************/
      
      if(doQuant){
        
        //loop over quantiles to calculate
        for(int whichQuant : numQuants ){
          
          //1/3 and 2/3 are to match algorithm 8 in the rstats quantile function
          //Not really sure what else is happening. I took this from R quantile function
          nppm = 1.0/3.0 + quantiles[whichQuant]*(numReps + 1.0/3.0);
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
              holdRow = IntegerVector(inputData.slice(slice).row(currentRow).begin(),
                                      inputData.slice(slice).row(currentRow).end());
              
              //sort vector
              std::sort(holdRow.begin(), holdRow.end());
              
              //Set vector with row, and an extra min/max. This is part of the formula,
              // It has been adapted here to avoid extra copies from push_front/bach
              test[0]=holdRow[0];
              test[seq(1,numReps)] = holdRow;
              test[numReps+1]=holdRow[numReps-1];
              
              //calculate quantiles. Also not sure why or how this works.
              printThing(currentRow,slice) = (1-h)*test[j] + h*test[j+1];
              
            }//end loop over rows
          }// end loop over slice
          
          // generate file name
          patchName = writeDir + sexNames[sex] + "Quantile_" + quantNames[whichQuant]
                                                      + "_Patch_"+new_string + ".csv";
          
          //Print out male stuff
          fwriteMain(wrap(printThing), wrap(patchName),
                     wrap(","), wrap("\n"), wrap("."), wrap(8));
          
        }//end loop over quantiles
        
      }//end quantiles chunk
      
    }//end patch loop
    
  }//loop over male/female sublist
  
}//end analysis function












//' @export
// [[Rcpp::export]]
void NEWANALYSIS(CharacterVector& maleNames, CharacterVector& femaleNames,
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