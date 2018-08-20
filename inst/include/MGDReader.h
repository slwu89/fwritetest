/*
  *      __  _____________       _       ______
  *     /  |/  / ____/ __ \_____(_)   __/ ____/
  *    / /|_/ / / __/ / / / ___/ / | / / __/
  *   / /  / / /_/ / /_/ / /  / /| |/ / /___
  *  /_/  /_/\____/_____/_/  /_/ |___/_____/
  *
  *  Marshall Lab
  *  August 2018
  *  File reader
  *
*/

#ifndef MGDRIVE_READ
#define MGDRIVE_READ

#include <iostream>
#include <fstream>
#include <string>
#include <vector>

//#include <Rcpp.h> // because used with armadillo later. can swap without armadillo
#include <RcppArmadillo.h>

class MGDReader {
public:
  
  // constructor & destructor
  MGDReader(const std::string& _femaleFile){
    
    // set size for holdString, get female file size
    holdStream.open(_femaleFile);  // open stream
    holdStream.seekg(0, std::ios::end);     // skip to end of file
    fileSize = holdStream.tellg();          // get size of file
    holdString.resize(fileSize*1.1);           // reserve space
    holdStream.close();
    
    // initialize holders for conversions
    baseDBL = 0.0;
    expDBL = 0.0;
    baseINT = 0;
    
  };
  
  ~MGDReader(){};
  
  // main functions
  void readFileDBL(const std::string& _file, const int& nrow, const int& ncol,
                   Rcpp::NumericMatrix& dataPlace);
  void readFileINT(const std::string& _file, const int& nrow, const int& ncol,
                   Rcpp::IntegerMatrix& dataPlace);
  
  // auxiliary functions
  double str2dbl(const char *p); // https://tinodidriksen.com/uploads/code/cpp/speed-string-to-double.cpp
  int str2int(const char *p);   // https://tinodidriksen.com/uploads/code/cpp/speed-string-to-int.cpp
  
  
private:
  // strings to hold input files
  std::string holdString;

  // input file stream
  std::ifstream holdStream;
  
  // file sizes
  size_t fileSize;
  
  // holder things for conversions
  double baseDBL;
  double expDBL;
  int    baseINT;
  size_t outerLoop;
  size_t innerLoop;
  
  std::string::iterator curChar;
  std::string oneNum;
  
};


/**************************************
 * Function Definitions
 **************************************/

// convert string to double, no signs
double MGDReader::str2dbl(const char *p) {
  baseDBL = 0.0;

  while (*p >= '0' && *p <= '9') {
    baseDBL = (baseDBL*10.0) + (*p - '0');
    ++p;
  }
  if (*p == '.') {
    expDBL = 0.0;
    ++p;
    while (*p >= '0' && *p <= '9') {
      expDBL = (expDBL*10.0) + (*p - '0');
      ++p;
    }
    // This is super specific to having 6 decimal places. 
    // if that changes, this has to change!!!!
    baseDBL += expDBL / 1000000; 
  }

  return baseDBL;
}

// convert string to int, no signs
int MGDReader::str2int(const char *p) {
  baseINT = 0;

  while (*p >= '0' && *p <= '9') {
    baseINT = (baseINT*10) + (*p - '0');
    ++p;
  }

  return baseINT;
}

// read doubles from file into an already created matrix
void MGDReader::readFileDBL(const std::string& _file, const int& nrow, const int& ncol,
                            Rcpp::NumericMatrix& dataPlace){
  
  holdString.clear();
  
  // open file
  holdStream.open(_file);
  // get amount to read
  holdStream.seekg(0, std::ios::end);
  fileSize = holdStream.tellg(); // size of file
  holdStream.seekg(0); // back to beginning

  // read in everything
  holdStream.read(&holdString[0], fileSize);
  // set iterator at beginning
  curChar = holdString.begin();

  // skip first line
  while(*&*curChar != '\n'){++curChar;}
  ++curChar;
  
  
  // loop over matrix, fill with things
  for(outerLoop=0; outerLoop < nrow; ++outerLoop){
    for(innerLoop=0; innerLoop < ncol; ++innerLoop){
      
      // clear string holder
      oneNum.clear();
      
      // get all characters part of this number
      while(*&*curChar != ',' && *&*curChar != '\n'){
        oneNum += *curChar;
        ++curChar;
      }
      // iterate over the comma or EOL
      ++curChar;
      
      // convert to double and put in matrix
      dataPlace(outerLoop, innerLoop) = str2dbl(&oneNum[0]);
      
    } // end loop over columns
  } // end loop over rows
  
  // close file
  holdStream.close();
  
} // end double reader

// read ints from file into an already created matrix
void MGDReader::readFileINT(const std::string& _file, const int& nrow, const int& ncol,
                            Rcpp::IntegerMatrix& dataPlace){
  
  holdString.clear();
  
  // open file
  holdStream.open(_file);
  // get amount to read
  holdStream.seekg(0, std::ios::end);
  fileSize = holdStream.tellg(); // size of file
  holdStream.seekg(0); // back to beginning
  
  // read in everything
  holdStream.read(&holdString[0], fileSize);
  // set iterator at beginning
  curChar = holdString.begin();
  
  // skip first line
  while(*&*curChar != '\n'){++curChar;}
  ++curChar;
  

  // loop over matrix, fill with things
  for(outerLoop=0; outerLoop < nrow; ++outerLoop){
    for(innerLoop=0; innerLoop < ncol; ++innerLoop){
      
      // clear string holder
      oneNum.clear();
      
      // get all characters part of this number
      while(*&*curChar != ',' && *&*curChar != '\n'){
        oneNum += *curChar;
        ++curChar;
      }
      // iterate over the comma or EOL
      ++curChar;
      
      // convert to double and put in matrix
      dataPlace(outerLoop, innerLoop) = str2int(&oneNum[0]);
      
    } // end loop over columns
  } // end loop over rows
  
  // close file
  holdStream.close();
  
} // end int reader



#endif
