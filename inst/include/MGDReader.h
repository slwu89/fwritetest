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

#include <Rcpp.h>


class MGDReader {
public:
  
  // constructor & destructor
  MGDReader(const std::string& _maleFile, const std::string& _femaleFile){
    
    Rcpp::Rcout << "Inside constructor" << std::endl;
    
    // set size for holdString, get female file size
    holdStream.open(_femaleFile);  // open stream
    std::getline(holdStream, holdFirstLine);  // store space for first line
    holdStream.seekg(0, std::ios::end);     // skip to end of file
    femaleSize = holdStream.tellg();          // get size of file
    holdString.resize(femaleSize);           // reserve space
    holdStream.close();
    
    // get male file size
    holdStream.open(_maleFile);
    holdStream.seekg(0, std::ios::end);
    maleSize = holdStream.tellg();
    holdStream.close();
    
    // initialize holders for conversions
    baseDBL = 0.0;
    expDBL = 0.0;
    baseINT = 0;
    

    
    
    
    // testing
    Rcpp::Rcout << "female file size: " << femaleSize << std::endl;
    Rcpp::Rcout << "String size: " << holdString.size() << std::endl;
    Rcpp::Rcout << "male file size: " << maleSize << std::endl;
    
  };
  
  ~MGDReader(){};
  
  // main functions
  void readFileDBL(const std::string& _file, const int& nrow, const int& ncol,
                   const int& fileSize, Rcpp::NumericMatrix *dataPlace);
  void readFileINT(const std::string& _file, const int& nrow, const int& ncol,
                   const int& fileSize, Rcpp::IntegerMatrix *dataPlace);
  
  // auxiliary functions
  double str2dbl(const char *p); // https://tinodidriksen.com/uploads/code/cpp/speed-string-to-double.cpp
  int str2int(const char *p);   // https://tinodidriksen.com/uploads/code/cpp/speed-string-to-int.cpp
  
  
private:
  // strings to hold input files
  std::string holdString;
  std::string holdFirstLine;
  
  // input file stream
  std::ifstream holdStream;
  
  // file sizes
  size_t femaleSize;
  size_t maleSize;
  
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
    baseINT = 0;
    ++p;
    while (*p >= '0' && *p <= '9') {
      expDBL = (expDBL*10.0) + (*p - '0');
      ++p;
      ++baseINT;
    }
    baseDBL += expDBL / std::pow(10.0, baseINT);
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
                            const int& fileSize, Rcpp::NumericMatrix *dataPlace){
  
  holdString.clear();
  
  // open file
  holdStream.open(_file);
  // throw away first line
  std::getline(holdStream, holdFirstLine);
  // read in everything
  holdStream.read(&holdString[0], fileSize);
  // set iterator at beginning
  curChar = holdString.begin();
  
  // loop over matrix, fill with things
  for(outerLoop=0; outerLoop < nrow; ++outerLoop){
    for(innerLoop=0; innerLoop < ncol; ++innerLoop){
      
      // clear string holder
      oneNum.clear();
      
      // get all characters part of this number
      while(*curChar != ',' || *curChar != '\n'){
        oneNum += *curChar;
        ++curChar;
      }
      // iterate over the comma or EOL
      ++curChar;
      
      
      
      
      
      
    } // end loop over columns
  } // end loop over rows
  
  // close file
  holdStream.close();
  
} // end double reader

// read ints from file into an already created matrix
void MGDReader::readFileINT(const std::string& _file, const int& nrow, const int& ncol,
                            const int& fileSize, Rcpp::IntegerMatrix *dataPlace){
  
  holdString.clear();
  
  // open file
  holdStream.open(_file);
  // throw away first line
  std::getline(holdStream, holdFirstLine);
  //read in everything
  holdStream.read(&holdString[0], fileSize);
  
  // loop over matrix, fill with things
  for(outerLoop=0; outerLoop < nrow; ++outerLoop){
    for(innerLoop=0; innerLoop < ncol; ++innerLoop){
      
      
      
      
      
      
      
      
    } // end loop over columns
  } // end loop over rows
  
  // close file
  holdStream.close();
  
} // end int reader























#endif









